# Counter

The Hello World tutorial showed the four `TuiApp` methods. This tutorial
takes the next step: a model that **changes over time** in response to
input. By the end you'll have a counter you can drive with keystrokes
and with a typed prompt.

The full source lives at
[`termflow.apps.counter.SyncCounter`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/counter/SyncCounter.scala)
in the sample module — launch with `sbt counterDemo` if you want to see
the finished version first.

## What you'll build

A small panel showing:

```text
┌──────────────────────────────────────┐
│ Current count: 3                     │
│                                      │
│ Commands:                            │
│   increment | + -> increase counter  │
│   decrement | - -> decrease counter  │
│   exit          -> quit              │
└──────────────────────────────────────┘
[]> _
```

Type `increment` (or `+`) at the prompt, hit Enter, the count goes up.
`decrement` / `-` decreases. `exit` quits.

## 1. Model the domain

We want the count to live in its own type so the rest of the app can
treat it as opaque. An opaque type alias is perfect:

```scala
opaque type Counter = Int

object Counter:
  def apply(count: Int): Counter = count

  extension (c: Counter)
    def count: Int                   = c
    def syncIncrement(): Counter     = Counter(c.count + 1)
    def syncDecrement(): Counter     = Counter(c.count - 1)
```

The application model wraps the counter together with the runtime bits
we need to keep alive — the keyboard subscription, the prompt state,
and the latest known terminal size:

```scala
final case class Model(
  terminalWidth: Int,
  terminalHeight: Int,
  counter: Counter,
  input: Sub[Msg],
  prompt: Prompt.State
)
```

`Sub[Msg]` and `Prompt.State` are stored on the model on purpose:

- The `Sub` keeps the keyboard pump alive for the lifetime of the app
  and gives the runtime something to cancel on exit.
- `Prompt.State` is the typed-but-not-yet-submitted buffer plus its
  cursor — every keystroke is funnelled through `Prompt.handleKey` and
  the new state is stored back on the model.

## 2. Define the message vocabulary

```scala
enum Msg:
  case Increment
  case Decrement
  case Exit
  case ConsoleInputKey(key: KeyDecoder.InputKey)
  case ConsoleInputError(error: Throwable)
```

Two interesting things compared to Hello World:

- **Domain messages and infra messages live in the same enum.**
  `Increment` / `Decrement` / `Exit` are what your domain cares about;
  `ConsoleInputKey` is a low-level key event the runtime delivers.
  Keeping both in the same `Msg` keeps `update` total — the compiler
  enforces that every event has a handler.
- **Errors get a Msg too.** `Sub.InputKey` accepts an `onError`
  callback; mapping errors into a regular `Msg` means `update` decides
  what to do (here: silently ignore).

## 3. Wire up `init`

```scala
override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  Model(
    terminalWidth  = ctx.terminal.width,
    terminalHeight = ctx.terminal.height,
    counter        = Counter(0),
    input          = Sub.InputKey(
                       msg     = ConsoleInputKey.apply,
                       onError = ConsoleInputError.apply,
                       ctx     = ctx
                     ),
    prompt         = Prompt.State()
  ).tui
```

`Sub.InputKey` constructor signatures take `msg: KeyDecoder.InputKey =>
Msg` and `onError: Throwable => Msg`. We can pass `ConsoleInputKey.apply`
and `ConsoleInputError.apply` directly — Scala 3 happily eta-expands the
case-class constructors into functions of the right shape.

The third argument, `ctx`, is the `RuntimeCtx[Msg]` — by passing it
here, the subscription **auto-registers** with the runtime, so we don't
need a `Cmd.RegisterSub`.

## 4. The shape of `update`

`update` is the only place state changes. With domain plus infra
messages it becomes a five-arm pattern match:

```scala
override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  val sized = syncTerminalSize(m, ctx)
  msg match
    case Increment =>
      sized.copy(counter = sized.counter.syncIncrement()).tui
    case Decrement =>
      sized.copy(counter = sized.counter.syncDecrement()).tui
    case Exit =>
      Tui(sized, Cmd.Exit)
    case ConsoleInputKey(k) =>
      val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](sized.prompt, k)(toMsg)
      maybeCmd match
        case Some(cmd) => Tui(sized.copy(prompt = nextPrompt), cmd)
        case None      => sized.copy(prompt = nextPrompt).tui
    case ConsoleInputError(_) => sized.tui
```

Walk through it:

- **`syncTerminalSize`** is a tiny helper that re-reads the terminal
  size from `ctx.terminal` and updates the model if it changed. We do
  this on every message, not just resize events — the cost is negligible
  and it catches the "terminal resized while no key was pressed" case.
- **`Increment` / `Decrement`** call into `Counter` and use the `.tui`
  extension to lift the new model into a `Tui[Model, Msg]` with no
  follow-up command.
- **`Exit`** pairs the model with `Cmd.Exit`, which tells the runtime to
  break out of the loop.
- **`ConsoleInputKey(k)`** delegates to `Prompt.handleKey`. The Prompt
  widget owns the cursor, history, paste, and everything else a real
  text input needs. It returns the next state plus an optional `Cmd`
  produced when the user pressed Enter — that's where `toMsg` runs.

## 5. Prompts and `toMsg`

`Prompt.handleKey` calls `toMsg(input: PromptLine)` whenever the user
presses Enter. The Prompt-widget API is "give me a function from a
submitted line to either an error or a Msg":

```scala
override def toMsg(input: PromptLine): Result[Msg] =
  Try {
    input.value.trim.toLowerCase match
      case "increment" | "+" => Increment
      case "decrement" | "-" => Decrement
      case "exit"            => Exit
  }.toEither.left.map(e => TermFlowError.Unexpected(e.getMessage))
```

`Result[A]` is `Either[TermFlowError, A]`. The `Try { … }.toEither` form
turns `MatchError` into a `Left` so a typo in the prompt surfaces a
validation message instead of crashing the runtime.

> If you want richer parsing — multiple commands, arguments, history —
> the Cookbook covers that. For a counter, three branches and a
> fall-through is plenty.

## 6. Drawing the panel with `Layout.Column`

Hand-positioning every `TextNode` doesn't scale beyond a few rows.
`Layout` lets you describe a vertical (or horizontal) stack and resolve
it to a list of children at a given coordinate:

```scala
val textColumn = Layout.Column(
  gap = 0,
  children = List(
    Layout.Elem(
      TextNode(
        1.x, 1.y,
        List(
          "Current count: ".text,
          Text(s"${m.counter.count}", renderCount(m.counter.count))
        )
      )
    ),
    Layout.Spacer(1, 1),
    Layout.Elem(TextNode(1.x, 1.y, List("Commands:".text(fg = Yellow)))),
    Layout.Elem(TextNode(1.x, 1.y, List("  increment | + -> increase counter".text))),
    Layout.Elem(TextNode(1.x, 1.y, List("  decrement | - -> decrease counter".text))),
    Layout.Elem(TextNode(1.x, 1.y, List("  exit          -> quit".text)))
  )
)
val textChildren = textColumn.resolve(Coord(2.x, 2.y))
```

Notes:

- `Layout.Elem` wraps a `VNode` so it lives in the layout tree.
- `Layout.Spacer(1, 1)` is a one-row vertical gap.
- The `(1.x, 1.y)` coordinates inside each `TextNode` are
  **layout-local** — the `resolve(Coord(2.x, 2.y))` call adds the
  column's origin so everything ends up in the right absolute spot.
- Style your text using the `.text` extension (`"foo".text(fg = Yellow)`)
  or build a `Text(string, style)` directly when you need different
  styles in one row.

## 7. Putting `view` together

```scala
override def view(m: Model): RootNode =
  val prefix         = "[]> "
  val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
  val boxWidth       = math.max(2, m.terminalWidth - 4)

  // … textColumn defined above …
  val textChildren = textColumn.resolve(Coord(2.x, 2.y))

  val border = BoxNode(
    1.x, 1.y, boxWidth, 6,
    children = Nil,
    style    = Style(border = true, fg = Blue)
  )

  RootNode(
    m.terminalWidth,
    13,
    children = border :: textChildren,
    input = Some(
      InputNode(
        2.x, 10.y,
        renderedPrompt.text,
        Style(fg = Green),
        cursor       = renderedPrompt.cursorIndex,
        prefixLength = renderedPrompt.prefixLength
      )
    )
  )
```

A few things to call out:

- **`BoxNode` is visual-only.** It draws a border but doesn't lay out
  its children. We keep it as a sibling of the resolved column rather
  than wrapping it.
- **Box width tracks the terminal.** `boxWidth = terminalWidth - 4` plus
  the resize sync from `update` means the panel grows and shrinks with
  the window.
- **`InputNode` is the prompt.** It carries the rendered prefix + buffer
  text, the cursor index, and the prefix length so `AnsiRenderer` can
  position the hardware cursor correctly. `Prompt.renderWithPrefix`
  returns all three in one shot.
- **`renderCount(c)`** is a tiny styling helper that picks a colour
  based on sign — black for zero, red for negative, green for positive.
  Live styling like that is one of the wins of having `view` be a pure
  function of the model.

## 8. Wiring it up

Same as Hello World:

```scala
def main(args: Array[String]): Unit =
  val _ = args
  TuiRuntime.run(App)
```

Run it:

```bash
sbt counterDemo
```

You can also drive the same logic from `update` keystrokes if you'd
rather not type commands. That extension is left as an exercise — the
cookbook covers it.

## What you've learned

- **State that changes over time.** `update` is a pattern match on
  `Msg` returning the next model.
- **Layout DSL.** `Layout.Column` + `resolve(at)` beats hand-positioning
  every node.
- **Prompts.** `Prompt.handleKey` turns key events into either a new
  buffer or a Submit-time `Cmd` driven by `toMsg`.
- **Adaptive sizing.** `syncTerminalSize` keeps `view` honest when the
  user resizes their window.

Next up: [Async work](03-async.md) — replacing the synchronous counter
with a `Cmd.FCmd` that does work on a background thread, plus a spinner
driven by `Sub.Every`.
