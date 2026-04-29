# Application layer

`termflow-app` is the layer most apps live in. It ties the terminal
and screen layers together with an Elm-style runtime, plus everything
you need to build a real interactive program: focus management,
keymaps, prompts, modal dialogs, async commands, timer subscriptions.

```scala
libraryDependencies += "org.llm4s" %% "termflow" % "0.2.0"
```

The umbrella `termflow` artefact pulls in the whole stack. Most apps
should depend on it rather than the four modules separately.

## `TuiApp[Model, Msg]`

The four-method contract:

```scala
trait TuiApp[Model, Msg]:
  def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg]
  def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg]
  def view(model: Model): RootNode
  def toMsg(input: PromptLine): Result[Msg]
```

The four tutorials cover this contract end-to-end:
[Hello, World](../tut/01-hello-world.md), [Counter](../tut/02-counter.md),
[Async work](../tut/03-async.md), and
[Forms and dialogs](../tut/04-forms-and-dialogs.md). This page
catalogues the surrounding pieces.

## `Tui[Model, Msg]`

```scala
final case class Tui[Model, Msg](model: Model, cmd: Cmd[Msg] = Cmd.NoCmd)

extension [Model](m: Model)
  def tui[Msg]: Tui[Model, Msg]                = Tui(m)
  def gCmd[Msg](msg: Msg): Tui[Model, Msg]     = Tui(m, Cmd.GCmd(msg))
```

A `Tui` is a model paired with a `Cmd`. `update` returns one. The
`.tui` and `.gCmd` extensions are the ergonomic way to construct
them inline.

## Cmd — effects

```scala
enum Cmd[+Msg]:
  case NoCmd                                                       extends Cmd[Nothing]
  case Exit                                                        extends Cmd[Nothing]
  case GCmd(msg: Msg)                                              extends Cmd[Msg]
  case FCmd[A, M](task: Future[A], toCmd: A => Cmd[M], onEnqueue: Option[M] = None) extends Cmd[M]
  case TermFlowErrorCmd(msg: TermFlowError)                        extends Cmd[Msg]
```

| Cmd | When |
|---|---|
| `NoCmd` | Pure state transitions (the common case). |
| `Exit` | Tear down the runtime, restore the terminal, return from `TuiRuntime.run`. |
| `GCmd(msg)` | Dispatch a follow-up `Msg` through `update`. Used to chain transitions. |
| `FCmd(task, toCmd, onEnqueue)` | Bridge a `Future[A]` into the runtime — covered in the [async tutorial](../tut/03-async.md). |
| `TermFlowErrorCmd(err)` | Surface a `TermFlowError` to the renderer (logged, not fatal). |

Plus the helper:

```scala
def Cmd.asyncResult[A, Msg](
  task:      AsyncResult[A],
  toCmd:     A => Cmd[Msg],
  onEnqueue: Option[Msg] = None
): Cmd[Msg]
```

`AsyncResult[A]` is `Future[Result[A]]`. Wrap fallible async work in
this and the runtime will fold a `Left(err)` into a
`TermFlowErrorCmd`.

## Sub — subscriptions

Subscriptions are how the outside world delivers events into
`update`. They run on background threads and publish `Cmd`s to the
runtime's command bus.

```scala
trait Sub[+Msg]:
  def isActive: Boolean
  def cancel(): Unit
  def start(): Unit
```

The factories you use day-to-day:

```scala
Sub.Every(millis: Long, msg: () => Msg, sink: EventSink[Msg]): Sub[Msg]

Sub.InputKey(
  msg:     KeyDecoder.InputKey => Msg,
  onError: Throwable => Msg,
  ctx:     RuntimeCtx[Msg]
): Sub[Msg]

Sub.TerminalResize(msg: () => Msg, sink: EventSink[Msg]): Sub[Msg]

val Sub.NoSub: Sub[Nothing]   // inert placeholder for model fields
```

When the third argument is a `RuntimeCtx[Msg]`, the sub
**auto-registers** for cleanup on `Cmd.Exit`. When it's a bare
`EventSink`, you're responsible for the lifecycle.

`.cancel()` is idempotent — safe to call on `NoSub` or on an
already-cancelled sub.

## RuntimeCtx — the ambient context

```scala
trait RuntimeCtx[Msg] extends EventSink[Msg]:
  def terminal:    TerminalBackend
  def config:      TermFlowConfig
  def registerSub(sub: Sub[Msg]): Sub[Msg]
  def publish(cmd: Cmd[Msg]): Unit
```

The runtime hands a `RuntimeCtx[Msg]` to `init` and to every `update`.
Use it to:

- read `terminal.width` / `terminal.height` for adaptive layout
- register subscriptions
- publish commands directly (rare — usually you return a `Cmd` from
  `update`)
- access `config` (logging, telemetry)

## TuiRuntime.run

```scala
TuiRuntime.run(app: TuiApp[Model, Msg])  // simplest form

TuiRuntime.run(
  app:             TuiApp[Model, Msg],
  renderer:        Option[TuiRenderer]    = None,
  terminalBackend: Option[TerminalBackend] = None,
  config:          Option[TermFlowConfig] = None
): Unit
```

The driver does, in order:

1. Open the terminal (alternate buffer, hide cursor, raw mode).
2. Set up the `CmdBus` queue.
3. Run `init`.
4. Loop: drain the bus, run `update` per message, run `view`,
   diff-render to ANSI, sleep up to ~60 fps.
5. On `Cmd.Exit`: cancel subs, restore terminal, return.

Pass a custom `TerminalBackend` for tests (the testkit's
`TestTerminalBackend` is what `TuiTestDriver` uses).

> Files: `Tui.scala`, `Sub.scala`, `TuiRuntime.scala`.

## FocusManager

```scala
opaque type FocusId = String

object FocusId:
  def apply(s: String): FocusId
  extension (id: FocusId) def value: String

final case class FocusManager(ids: Vector[FocusId], current: Option[FocusId]):
  def isFocused(id: FocusId):    Boolean
  def next:                      FocusManager
  def previous:                  FocusManager
  def focus(id: FocusId):        FocusManager
  def clear:                     FocusManager
  def withIds(newIds: Vector[FocusId]): FocusManager
```

Pure value, immutable transitions, wraps at either end. Construct via
`FocusManager(Vector(NameId, EmailId))` — the first id becomes the
initial focus.

The forms tutorial shows the full pattern:
[per-step `FocusManager`s](../tut/04-forms-and-dialogs.md#3-a-focusmanager-per-step),
[focus dispatch](../tut/04-forms-and-dialogs.md#6-focus-dispatch),
[explicit `focus(id)`](../tut/04-forms-and-dialogs.md#9-the-plan-step--radiogroup).

> File: `FocusManager.scala`.

## Dialogs — modal overlays

`Dialogs` builds `Overlay` values. Mount them on `RootNode.overlays`
and the renderer composites them on top of the rest of the frame.

```scala
import termflow.tui.Dialogs

Dialogs.message(title, body: List[String], choices: List[Choice], position, theme): Overlay
Dialogs.confirm(prompt: String, yesFocused: Boolean, …): Overlay
Dialogs.textInput(title, prompt, value, cursor, okFocused, …): Overlay
Dialogs.listSelect[A](title, items, selectedIdx, visibleRows, itemLabel, …): Overlay
Dialogs.waiting(title, message, …): Overlay
Dialogs.fileDialog(basePath, onSelect, onCancel, …): Overlay
Dialogs.directoryDialog(basePath, onSelect, onCancel, …): Overlay
Dialogs.actionList[A](title, items, itemLabel, onSelect, …): Overlay
```

Every helper takes a `position` and an implicit `Theme`. The
showcase's *Dialogs* tab exercises all of them — see
[`Stage1ShowcaseApp`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/showcase/Stage1ShowcaseApp.scala)
for working examples of each one.

> File: `Dialogs.scala`.

## Prompt — single-line text input

```scala
final case class Prompt.State(buffer: Vector[Char] = Vector.empty, cursor: Int = 0)

def Prompt.handleKey[G](state: State, k: InputKey)(toMsg: PromptLine => Result[G])
  : (State, Option[Cmd[G]])

def Prompt.renderWithPrefix(state: State, prefix: String): RenderedLine

def Prompt.cursorColumn(state: State): Int
```

`handleKey` is grapheme-aware — Backspace deletes a whole cluster, not
a code point. `renderWithPrefix` returns the rendered text plus the
cursor index and prefix length so you can hand it straight to an
`InputNode`. `cursorColumn` uses `WCWidth` for the column position
(matters for CJK / emoji inputs).

> File: `Prompt.scala`.

## TuiPrelude — the import you always want

```scala
import termflow.tui.TuiPrelude.*

opaque type PromptLine = String
type Result[A]         = Either[TermFlowError, A]
type AsyncResult[+A]   = Future[Result[A]]

def AsyncResult.success[A](value: A): AsyncResult[A]
def AsyncResult.failure[A](err: TermFlowError): AsyncResult[A]
def AsyncResult.fromResult[A](r: Result[A]): AsyncResult[A]
def AsyncResult.fromFuture[A](task: Future[A])(using ec: ExecutionContext): AsyncResult[A]
```

Plus the screen-prelude conversions (`.x`, `.y`, `.text`).

`TermFlowError` is the closed error ADT:
`ConfigError | ModelNotFound | Unexpected | Validation | CommandError | UnknownApp`.

> File: `TuiPrelude.scala`.

## Where to next

- **Widgets.** The component catalogue: [Widgets guide](widgets.md).
- **Keymaps.** Replace ad-hoc match-on-key with declarative bindings:
  [Keymap guide](keymap.md).
- **Theming.** Override colours and box-drawing glyphs:
  [Theming guide](theming.md).
- **Testing.** Drive the runtime synchronously in tests:
  [Testing guide](testing.md).

The full per-type API is in the [Scaladoc](../reference/api.md).
