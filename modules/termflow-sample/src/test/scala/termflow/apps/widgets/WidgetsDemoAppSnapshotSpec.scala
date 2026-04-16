package termflow.apps.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.widgets.WidgetsDemoApp.CancelFocus
import termflow.apps.widgets.WidgetsDemoApp.Msg
import termflow.apps.widgets.WidgetsDemoApp.SaveFocus
import termflow.testkit.TuiTestDriver
import termflow.tui.*

/**
 * Integration coverage for the widgets demo.
 *
 * Instead of full-frame golden snapshots — which would be flaky against the
 * `Sub.Every` timer race in `init` (the scheduler fires its initial tick on a
 * background thread before `TuiTestDriver` can cancel it; tracked in
 * `llm4s/termflow#92`) — this spec drives the demo synchronously and asserts
 * on cell-level properties that are invariant to the tick count: which
 * button is focused, what style the status row is painted in, and whether
 * `Cmd.Exit` is observed on quit.
 */
class WidgetsDemoAppSnapshotSpec extends AnyFunSuite:

  private val Width  = 80
  private val Height = 14

  private def driver(): TuiTestDriver[WidgetsDemoApp.Model, WidgetsDemoApp.Msg] =
    val d = TuiTestDriver(WidgetsDemoApp.App, width = Width, height = Height)
    d.init()
    d

  private def statusBg(d: TuiTestDriver[WidgetsDemoApp.Model, WidgetsDemoApp.Msg]): Color =
    val frame = d.frame
    // Status bar is the last row; the first cell of the " demo " tag is enough.
    frame.cells(Height - 1)(1).style.bg

  test("initial model is on Save focus and dark theme"):
    val d = driver()
    assert(d.model.fm.isFocused(SaveFocus))
    assert(d.model.darkTheme)
    assert(statusBg(d) == Theme.dark.primary)

  test("Msg.NextFocus cycles focus forward through the FocusManager"):
    val d = driver()
    d.send(Msg.NextFocus)
    assert(d.model.fm.isFocused(CancelFocus))
    d.send(Msg.NextFocus)
    assert(d.model.fm.isFocused(SaveFocus)) // wraps

  test("Msg.PrevFocus cycles focus backward (and wraps)"):
    val d = driver()
    d.send(Msg.PrevFocus)
    assert(d.model.fm.isFocused(CancelFocus))

  test("Msg.Activate records the label of the currently focused button"):
    val d = driver()
    d.send(Msg.NextFocus) // focus -> Cancel
    d.send(Msg.Activate)
    assert(d.model.lastAction == "clicked Cancel")

  test("Msg.ToggleTheme repaints the status row in the light palette"):
    val d = driver()
    assert(statusBg(d) == Theme.dark.primary)
    d.send(Msg.ToggleTheme)
    assert(!d.model.darkTheme)
    assert(statusBg(d) == Theme.light.primary)

  test("Msg.BumpProgress clamps to [0, 1]"):
    val d  = driver()
    val p0 = d.model.progress
    d.send(Msg.BumpProgress(-1.0))
    assert(d.model.progress == 0.0)
    d.send(Msg.BumpProgress(5.0))
    assert(d.model.progress == 1.0)
    // Ensure the starting progress didn't prevent the clamp round-trip.
    assert(p0 >= 0.0 && p0 <= 1.0)

  test("Msg.Quit publishes Cmd.Exit"):
    val d = driver()
    d.send(Msg.Quit)
    assert(d.exited)

  test("button row reflects focus by painting Save bold when focused, Cancel plain"):
    val d     = driver()
    val frame = d.frame
    val rowIdx = (0 until frame.height).indexWhere { r =>
      val s = (0 until frame.width).map(c => frame.cells(r)(c).ch).mkString
      s.contains("[ Save ]") && s.contains("[ Cancel ]")
    }
    assert(rowIdx >= 0, "expected a row containing both button labels")
    val row    = frame.cells(rowIdx)
    val savIdx = (0 until frame.width).find(c => row(c).ch == 'S').get
    val canIdx = (0 until frame.width).find(c => row(c).ch == 'C').get
    assert(row(savIdx).style.bold, "Save label should be bold (focused)")
    assert(!row(canIdx).style.bold, "Cancel label should not be bold when Save is focused")

  test("ConsoleInputKey dispatches via the Keymap — Tab cycles focus"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(KeyDecoder.InputKey.Ctrl('I')))
    assert(d.model.fm.isFocused(CancelFocus))

  test("ConsoleInputKey dispatches via the Keymap — q quits"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(KeyDecoder.InputKey.CharKey('q')))
    assert(d.exited)

  test("the Keymap binds every documented shortcut"):
    // Spot-check the binding table directly so future bindings can't be
    // dropped without breaking this test.
    val k = WidgetsDemoApp.Keys
    import KeyDecoder.InputKey.*
    assert(k.lookup(Ctrl('I')).contains(Msg.NextFocus))
    assert(k.lookup(Enter).contains(Msg.Activate))
    assert(k.lookup(CharKey(' ')).contains(Msg.Activate))
    assert(k.lookup(CharKey('t')).contains(Msg.ToggleTheme))
    assert(k.lookup(CharKey('+')).contains(Msg.BumpProgress(0.1)))
    assert(k.lookup(CharKey('-')).contains(Msg.BumpProgress(-0.1)))
    assert(k.lookup(CharKey('q')).contains(Msg.Quit))
    assert(k.lookup(Ctrl('C')).contains(Msg.Quit))
    assert(k.lookup(Escape).contains(Msg.Quit))
