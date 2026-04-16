package termflow.apps.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.widgets.WidgetsDemoApp.Focus
import termflow.apps.widgets.WidgetsDemoApp.Msg
import termflow.testkit.TuiTestDriver
import termflow.tui.*

/**
 * Integration coverage for the widgets demo.
 *
 * Instead of full-frame golden snapshots — which would be flaky against the
 * `Sub.Every` timer race in `init` (the scheduler fires its initial tick on a
 * background thread before `TuiTestDriver` can cancel it) — this spec drives
 * the demo synchronously and asserts on cell-level properties that are
 * invariant to the tick count: which button is focused, what style the
 * status row is painted in, and whether `Cmd.Exit` is observed on quit.
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
    assert(d.model.focus == Focus.Save)
    assert(d.model.darkTheme)
    assert(statusBg(d) == Theme.dark.primary)

  test("Msg.NextFocus flips the model focus"):
    val d = driver()
    d.send(Msg.NextFocus)
    assert(d.model.focus == Focus.Cancel)
    d.send(Msg.NextFocus)
    assert(d.model.focus == Focus.Save)

  test("Msg.Activate records the currently focused action"):
    val d = driver()
    d.send(Msg.NextFocus)
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
    // The buttons row is the third non-blank row in the layout; layout ordering
    // is: title (row 2), spinner+bar (row 4), buttons (row 6). Locate 'S' of
    // "Save" and 'C' of "Cancel" by scanning for them.
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
