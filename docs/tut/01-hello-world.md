# Hello, World

This tutorial walks through the smallest TermFlow app worth writing: a
window that prints "Hello, TermFlow!" and exits when you press `q` or
`Ctrl-C`.

By the end of it you'll have:

- a runnable Scala 3 sbt project,
- a single `TuiApp` definition,
- enough vocabulary to follow the rest of the tutorials.

> Reading time: ~10 minutes. Working code is at the bottom of the page if
> you want to skim first.

## 1. Set up a project

```text
hello-termflow/
├─ build.sbt
└─ src/main/scala/HelloApp.scala
```

`build.sbt`:

```scala
ThisBuild / scalaVersion := "3.7.1"

libraryDependencies += "org.llm4s" %% "termflow" % "0.2.0"
```

If you've never used sbt before, install it via
[`coursier`](https://get-coursier.io) (`cs install sbt`) or
[`sdkman`](https://sdkman.io/) (`sdk install sbt`).

## 2. Anatomy of a `TuiApp`

Every TermFlow program implements a single trait,
[`TuiApp[Model, Msg]`](../reference/api.md), with four methods:

| Method | Purpose |
|---|---|
| `init(ctx)`           | Build the initial model and an optional startup `Cmd`. |
| `update(m, msg, ctx)` | Apply a `Msg` to the model and return the next state. |
| `view(m)`             | Render the model as a virtual DOM (`RootNode`). |
| `toMsg(input)`        | Convert a submitted prompt line into a `Msg`. |

`Model` is your application state (any type — typically a `case class`).
`Msg` is the closed set of events your app reacts to (any sealed type —
typically a Scala 3 `enum`).

Inside the runtime:

1. `init` runs once at startup.
2. `update` runs every time a `Msg` arrives.
3. `view` runs after every `update`, producing a frame.

Three pure functions plus the runtime — that is the whole surface.

## 3. Decide on a model and messages

The model needs the message we want to display, plus a reference to the
keyboard subscription (so it stays live for the lifetime of the app and
can be cancelled cleanly on exit):

```scala
final case class Model(message: String, input: Sub[Msg])
```

For messages we need two — a key event and an exit signal:

```scala
enum Msg:
  case KeyPressed(key: KeyDecoder.InputKey)
  case Quit
```

## 4. Subscribe to keys in `init`

Subscriptions are the bridge from "things that happen outside `update`"
(timers, keystrokes, async results) into your `update` function.

`Sub.InputKey(...)` constructs a keyboard subscription: every keypress is
turned into a `Msg` and pushed onto the runtime's command bus. When you
pass the `RuntimeCtx` as the third argument, the subscription
auto-registers itself with the runtime — no `Cmd.RegisterSub` plumbing
needed:

```scala
def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  val keys = Sub.InputKey[Msg](
    msg     = key => Msg.KeyPressed(key),
    onError = _   => Msg.Quit,
    ctx     = ctx
  )
  Model("Hello, TermFlow!", keys).tui
```

The `.tui` extension lifts a model into a `Tui[Model, Msg]` with no
follow-up command — equivalent to `Tui(model, Cmd.NoCmd)`.

## 5. Handle keys in `update`

Pattern-match on the inbound `Msg` to decide what to do:

```scala
def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Msg.KeyPressed(KeyDecoder.InputKey.CharKey('q')) => Tui(m, Cmd.Exit)
    case Msg.KeyPressed(KeyDecoder.InputKey.Ctrl('C'))    => Tui(m, Cmd.Exit)
    case Msg.KeyPressed(_)                                => m.tui
    case Msg.Quit                                         => Tui(m, Cmd.Exit)
```

`Cmd.Exit` tells the runtime to break out of the loop, cancel
subscriptions, restore the terminal, and return.

## 6. Draw something

`view` returns a `RootNode` — a width-by-height container holding child
nodes. The simplest child is a `TextNode` placed at a coordinate.
TermFlow's renderer diffs the new `RootNode` against the previous frame
and emits the minimum ANSI sequence needed.

```scala
def view(m: Model): RootNode =
  RootNode(
    width    = 40,
    height   = 3,
    children = List(
      TextNode(2.x, 1.y, List(m.message.text))
    ),
    input    = None
  )
```

Notes:

- `2.x` and `1.y` come from `import termflow.tui.TuiPrelude.*` — they are
  extension conversions to the opaque `XCoord` / `YCoord` types so you
  can't accidentally swap rows and columns. Coordinates are 1-based and
  frame-local.
- `m.message.text` lifts the `String` into a `Text(value, Style.empty)`
  fragment. The `text` extension lives in `TuiPrelude` too.

## 7. Implement `toMsg`

`TuiApp` requires a `toMsg` even though our hello-world app never
submits a prompt line. We can return `Quit` for everything:

```scala
def toMsg(input: PromptLine): Result[Msg] = Right(Msg.Quit)
```

`Result[A]` is `Either[TermFlowError, A]`, defined in `TuiPrelude`.

## 8. Run it

Wire everything into a `main`:

```scala
def main(args: Array[String]): Unit =
  val _ = args
  TuiRuntime.run(App)
```

`TuiRuntime.run` switches the terminal into the alternate buffer, sets
up the `CmdBus`, runs `init`, and starts the loop. On `Cmd.Exit` it
restores the terminal and returns.

```bash
sbt run
```

You should see `Hello, TermFlow!` near the top-left of the window. Press
`q` or `Ctrl-C` to exit.

> If the terminal looks corrupted after exit, run `reset` — that's a
> sign the app crashed before `Cmd.Exit` could restore the terminal
> state. Wrapping `TuiRuntime.run` is rarely necessary; the runtime has
> a shutdown hook that handles SIGINT gracefully.

## Full source

```scala
import termflow.tui.*
import termflow.tui.Tui.*           // brings the .tui extension into scope
import termflow.tui.TuiPrelude.*    // brings .x, .y, .text into scope

object HelloApp:

  final case class Model(message: String, input: Sub[Msg])

  enum Msg:
    case KeyPressed(key: KeyDecoder.InputKey)
    case Quit

  object App extends TuiApp[Model, Msg]:

    def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val keys = Sub.InputKey[Msg](
        msg     = key => Msg.KeyPressed(key),
        onError = _   => Msg.Quit,
        ctx     = ctx
      )
      Model("Hello, TermFlow!", keys).tui

    def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Msg.KeyPressed(KeyDecoder.InputKey.CharKey('q')) => Tui(m, Cmd.Exit)
        case Msg.KeyPressed(KeyDecoder.InputKey.Ctrl('C'))    => Tui(m, Cmd.Exit)
        case Msg.KeyPressed(_)                                => m.tui
        case Msg.Quit                                         => Tui(m, Cmd.Exit)

    def view(m: Model): RootNode =
      RootNode(
        width    = 40,
        height   = 3,
        children = List(
          TextNode(2.x, 1.y, List(m.message.text))
        )
      )

    def toMsg(input: PromptLine): Result[Msg] = Right(Msg.Quit)

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)
```

## What's next?

You have now seen the whole `TuiApp` contract — `init`, `update`, `view`,
`toMsg`. The next tutorial swaps the static message for a model that
changes over time:

→ [Counter tutorial](02-counter.md)
