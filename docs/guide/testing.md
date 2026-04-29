# Testing

`termflow-testkit` is the test-only companion to `termflow-app`. It
gives you a synchronous driver that runs your `TuiApp` without a real
terminal, captures rendered frames as cell matrices for snapshot
testing, and provides `KeySim` / `MouseSim` constructors so you don't
have to hand-build `KeyDecoder.InputKey` values in test code.

```scala
libraryDependencies += "org.llm4s" %% "termflow-testkit" % "0.2.0" % Test
```

## TuiTestDriver

The single entry point for testing apps:

```scala
import termflow.testkit.{TuiTestDriver, KeySim}
import termflow.tui.KeyDecoder.InputKey

val driver = TuiTestDriver(MyApp.App, width = 80, height = 24)
driver.init()

driver.send(MyApp.Msg.Increment)
assert(driver.model.count == 1)
```

The driver:

- Runs `app.init(ctx)` synchronously, captures the `Tui[Model, Msg]`.
- Drains the command bus on every `send`, processing `GCmd`s
  recursively so chained transitions complete before `send` returns.
- Renders the current model after every `send` — `driver.frame`
  returns the latest `RenderFrame`.
- Suppresses subscription start-up — `Sub.Every` timers never tick,
  `Sub.InputKey` never reads stdin, so tests stay deterministic.

Key methods:

```scala
class TuiTestDriver[Model, Msg]:
  def init():    Unit
  def send(msg:  Msg):  Unit
  def model:     Model
  def exited:    Boolean
  def cmds:      List[Cmd[Msg]]
  def observedErrors: List[TermFlowError]
  def frame:     RenderFrame
```

## Asserting on the frame

The simplest assertion is "did the rendered text contain X?":

```scala
val rendered = (0 until driver.frame.height)
  .map(r => driver.frame.cells(r).map(_.ch).mkString)
  .mkString("\n")

assert(rendered.contains("count: 1"))
```

For style assertions, drop into `RenderCell`:

```scala
val cell = driver.frame.cells(2)(15)
assert(cell.ch == '✓')
assert(cell.style.fg == Color.Green)
```

## Golden snapshots

For higher-fidelity tests, mix in `GoldenSupport`:

```scala
import termflow.testkit.GoldenSupport
import org.scalatest.funsuite.AnyFunSuite

class MyAppSpec extends AnyFunSuite with GoldenSupport:
  override def goldenSuiteName = "MyApp"

  test("initial frame matches golden") {
    val d = TuiTestDriver(MyApp.App, width = 80, height = 24)
    d.init()
    assertGoldenFrame(d.frame, "initial")
  }
```

First run: the golden file doesn't exist, the test fails. Run with
`-Dtermflow.update-goldens=true` (or `UPDATE_GOLDENS=1`) and the
driver writes it. Commit the golden, future runs assert against it.

Goldens live under
`src/test/resources/termflow/golden/<suite>/<name>.txt` by default.
Override via `goldenDir` and `goldenPath(name)`.

## KeySim — typed key constructors

```scala
import termflow.testkit.KeySim

KeySim.Tab           // InputKey.Tab
KeySim.Enter         // InputKey.Enter
KeySim.Escape        // InputKey.Escape

KeySim.char('q')     // InputKey.CharKey('q')
KeySim.ctrl('S')     // InputKey.Ctrl('S')
KeySim.f(2)          // InputKey.F2
KeySim.paste("hi")   // InputKey.Paste("hi")
```

Modifier wrappers:

```scala
KeySim.shift(KeySim.Tab)              // BackTab equivalent (Shift+Tab)
KeySim.ctrl(KeySim.char('c'))         // Ctrl+C
KeySim.modified(InputKey.End, shift = true, ctrl = true)
```

Sequences:

```scala
KeySim.typeString("hello")
// → List(CharKey('h'), CharKey('e'), CharKey('l'), CharKey('l'), CharKey('o'))

KeySim.typeString("a\tb")
// → List(CharKey('a'), Tab, CharKey('b'))   // \t becomes Tab

KeySim.typeString("a\nb")
// → List(CharKey('a'), Enter, CharKey('b')) // \n becomes Enter
```

This is what the showcase, wizard, and form-demo specs use to drive
their drivers — typing a string and asserting on the resulting model
is by far the most readable test shape.

## MouseSim — typed mouse constructors

Mouse events live inside `InputKey.Mouse(event)`. `MouseSim` wraps
that for you:

```scala
import termflow.testkit.MouseSim

MouseSim.click(col = 10, row = 5)               // Press at (10, 5)
MouseSim.scrollOnce(ScrollDirection.Down, 10, 5) // Single wheel click

MouseSim.clickPair(col = 10, row = 5)
// → Vector(InputKey.Mouse(Press(...)), InputKey.Mouse(Release(...)))

MouseSim.scroll(ScrollDirection.Down, 10, 5, ticks = 3)
// → three Mouse(Scroll(Down, ...)) events
```

For drag-resize tests of `SplitPane`:

```scala
val events = Vector(
  MouseSim.press(col = 40, row = 12),
  MouseSim.drag(col = 50, row = 12),
  MouseSim.release(col = 50, row = 12)
)
events.foreach(e => driver.send(MyApp.Msg.KeyPressed(e)))
```

## TestRuntimeCtx

`TuiTestDriver` constructs a `TestRuntimeCtx[Msg]` for you, but if
you want to drive a `TuiApp` lower-level, the ctx is exposed
directly. Useful when you're testing a sub-component that takes a
`RuntimeCtx[Msg]` parameter without going through the full app.

```scala
val ctx = TestRuntimeCtx[MyApp.Msg](width = 80, height = 24)

// Register a sub manually if you need to assert on it:
val keys = Sub.InputKey[MyApp.Msg](
  msg     = k => MyApp.Msg.KeyPressed(k),
  onError = _ => MyApp.Msg.Quit,
  ctx     = ctx
)
assert(ctx.registeredSubs.contains(keys))
```

## A complete spec

```scala
import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.{TuiTestDriver, KeySim}

class CounterSpec extends AnyFunSuite:

  private def driver = {
    val d = TuiTestDriver(SyncCounter.App, width = 80, height = 24)
    d.init()
    d
  }

  test("increment via keystroke") {
    val d = driver
    KeySim.typeString("increment").foreach(k =>
      d.send(SyncCounter.Msg.ConsoleInputKey(k))
    )
    d.send(SyncCounter.Msg.ConsoleInputKey(KeySim.Enter))
    assert(d.model.counter.count == 1)
  }

  test("exit on Ctrl+C") {
    val d = driver
    d.send(SyncCounter.Msg.ConsoleInputKey(KeySim.ctrl('C')))
    assert(d.exited)
  }
```

## Testing async work

`Cmd.FCmd` futures don't run automatically inside the driver — that
would make tests non-deterministic. Instead, the driver records the
`FCmd` in `cmds` and you assert on it:

```scala
test("Increment dispatches an FCmd with onEnqueue = Busy") {
  val d = driver
  d.send(FutureCounter.Msg.Increment)
  val fcmd = d.cmds.collectFirst {
    case f: Cmd.FCmd[?, ?] => f
  }.getOrElse(fail("expected an FCmd"))
  assert(fcmd.onEnqueue.exists(_.isInstanceOf[FutureCounter.Msg.Busy]))
}
```

For end-to-end async tests, use `Await.result` on the captured future
and feed the resolved value back via `driver.send`. This is the
pattern the `FutureCounter` spec uses.

## Reference

> Files: `modules/termflow-testkit/src/main/scala/termflow/testkit/{TuiTestDriver,TestRuntimeCtx,KeySim,MouseSim,GoldenSupport}.scala`.

For working examples, every spec under
[`modules/termflow-sample/src/test/scala`](https://github.com/llm4s/termflow/tree/main/modules/termflow-sample/src/test/scala)
uses the testkit — the wizard, showcase, form-demo, and dashboard
specs cover the full surface.
