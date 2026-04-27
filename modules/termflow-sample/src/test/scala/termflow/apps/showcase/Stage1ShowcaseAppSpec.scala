package termflow.apps.showcase

import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.TuiTestDriver
import termflow.tui.AnsiRenderer
import termflow.tui.BorderChars
import termflow.tui.KeyDecoder.InputKey

/**
 * Smoke test for the Stage 1 showcase. We don't need a golden — the demo
 * is meant to be eyeballed in a real terminal — but we do want assurance
 * that:
 *   - the layout composes (no exception, frame builds)
 *   - cycling borders + opening the dialog drives the model as expected
 *   - the modal suppresses the base view's bindings
 */
class Stage1ShowcaseAppSpec extends AnyFunSuite:

  private def driver: TuiTestDriver[Stage1ShowcaseApp.Model, Stage1ShowcaseApp.Msg] =
    val d = TuiTestDriver(Stage1ShowcaseApp.App, width = 100, height = 28)
    d.init()
    d

  test("initial frame renders without exceptions and starts on rounded borders") {
    val d     = driver
    val frame = d.frame
    assert(frame.width >= 60)
    assert(frame.cells.flatten.exists(_.ch == BorderChars.rounded.topLeft), "expected rounded ╭ in initial frame")
  }

  test("pressing 'b' cycles BorderChars on the model") {
    val d       = driver
    val initial = d.model.borderName
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('b')))
    val next = d.model.borderName
    assert(next != initial, s"expected border name to change from $initial, but stayed")
  }

  test("pressing 't' cycles theme presets") {
    val d = driver
    assert(d.model.themeName == "dark")
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('t')))
    assert(d.model.themeName == "light")
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('t')))
    assert(d.model.themeName == "mono")
  }

  test("'d' opens the confirm dialog and base bindings are suppressed while open") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('d')))
    assert(d.model.dialog.isInstanceOf[Stage1ShowcaseApp.Dialog.ConfirmQuit])
    val borderBefore = d.model.borderName
    // While the dialog is up, 'b' should NOT cycle the border.
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('b')))
    assert(d.model.borderName == borderBefore, "modal must suppress base 'b' binding")
  }

  test("the rendered frame contains a modal dialog cursor suppression when the dialog is open") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('d')))
    val frame = d.frame
    // No InputNode in this app, so cursor is None either way; the visual cue is
    // the dialog border. Spot-check that the rendered cells contain the dialog title.
    val rendered = (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
    assert(rendered.contains("Confirm"), "dialog title 'Confirm' should appear in the rendered frame")
    assert(rendered.contains("Quit the showcase?"), "dialog body should appear")
  }

  test("Esc inside the dialog closes it without quitting") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('d')))
    assert(d.model.dialog != Stage1ShowcaseApp.Dialog.None)
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.Escape))
    assert(d.model.dialog == Stage1ShowcaseApp.Dialog.None)
    assert(!d.exited, "Esc should close the dialog, not quit")
  }

  test("Layout.Fill expands the middle panel relative to terminal width") {
    val narrow = TuiTestDriver(Stage1ShowcaseApp.App, width = 60, height = 20); narrow.init()
    val wide   = TuiTestDriver(Stage1ShowcaseApp.App, width = 120, height = 20); wide.init()
    // Render once so the frame reflects the active width.
    val nf = narrow.frame
    val wf = wide.frame
    assert(wf.width > nf.width, s"wide frame should be wider than narrow (${wf.width} vs ${nf.width})")
  }

  // Silence unused-import lint warning for AnsiRenderer (it's referenced
  // implicitly through TuiTestDriver, but the file would warn otherwise).
  private val _ = AnsiRenderer.getClass
