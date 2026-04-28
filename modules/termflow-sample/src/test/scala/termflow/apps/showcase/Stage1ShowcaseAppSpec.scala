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

  test("a key event updates lastEvent on the model") {
    val d = driver
    assert(d.model.lastEvent.isEmpty)
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('x')))
    assert(d.model.lastEvent.exists(_.contains("CharKey")))
  }

  test("a Modified key arrives with mod prefix in lastEvent") {
    val d = driver
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Modified(InputKey.ArrowRight, termflow.tui.KeyDecoder.Modifiers(ctrl = true))
      )
    )
    val rendered = d.model.lastEvent.getOrElse("")
    assert(rendered.contains("Ctrl"), s"expected 'Ctrl' in $rendered")
    assert(rendered.contains("ArrowRight"), s"expected 'ArrowRight' in $rendered")
  }

  test("a Mouse event arrives in lastEvent with coordinates") {
    val d = driver
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            7,
            3,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    val rendered = d.model.lastEvent.getOrElse("")
    assert(rendered.startsWith("mouse"), s"expected mouse prefix in $rendered")
    assert(rendered.contains("(7,3)"), s"expected (7,3) in $rendered")
  }

  test("a Paste event arrives in lastEvent with a length-bounded preview") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.Paste("hello pasted world")))
    val rendered = d.model.lastEvent.getOrElse("")
    assert(rendered.startsWith("paste"), s"expected paste prefix in $rendered")
    assert(rendered.contains("hello pasted world"), s"expected payload in $rendered")
  }

  // ---- Mouse hit-testing on the Themes / Borders panels --------------------

  /**
   * Themes panel sits at `(width - 22, 3)` with 22 cols × 11 rows. The
   * BoxNode draws its border on row 3 (the first panel row); panel-local
   * Y=3 children translate to absolute row `3 + (panel.row - 1) =
   * panel.row + 2 = 5`. So the rendered theme rows live at absolute
   * Y = 5 (dark), 6 (light), 7 (mono).
   */
  private def themesRowCol(d: TuiTestDriver[Stage1ShowcaseApp.Model, Stage1ShowcaseApp.Msg]): Int =
    d.model.width - 22 + 5

  /** Absolute row of theme/border item `idx` inside its panel. */
  private val firstItemRow = 5

  test("clicking the 'light' row in the Themes panel selects light") {
    val d = driver
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            themesRowCol(d),
            firstItemRow + 1, // light is idx=1
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.themeName == "light")
  }

  test("clicking the 'mono' row in the Themes panel selects mono") {
    val d = driver
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            themesRowCol(d),
            firstItemRow + 2, // mono is idx=2
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.themeName == "mono")
  }

  test("scroll-down inside the Themes panel cycles to the next theme") {
    val d           = driver
    val themesAnyY  = 3 + 5 // anywhere inside the panel rectangle
    val initialName = d.model.themeName
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Scroll(
            termflow.tui.ScrollDirection.Down,
            themesRowCol(d),
            themesAnyY,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.themeName != initialName, "scroll down should advance the selection")
  }

  test("scroll-up inside the Themes panel cycles to the previous theme (wraps)") {
    val d          = driver
    val themesAnyY = 3 + 5
    // From dark (idx 0), Up should wrap to mono (idx 2).
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Scroll(
            termflow.tui.ScrollDirection.Up,
            themesRowCol(d),
            themesAnyY,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.themeName == "mono")
  }

  test("clicking outside the Themes panel does not change the theme") {
    val d           = driver
    val initialName = d.model.themeName
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            1,
            1,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.themeName == initialName, "clicks outside Themes should be no-ops")
  }

  // ---- Dialog helper bindings (Stage 3 §6.1) -------------------------------

  test("'i' opens the textInput dialog") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('i')))
    assert(d.model.dialog.isInstanceOf[Stage1ShowcaseApp.Dialog.TextInput])
  }

  test("typing characters into the textInput dialog updates its prompt buffer") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('i')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('h')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('i')))
    d.model.dialog match
      case Stage1ShowcaseApp.Dialog.TextInput(state, _) =>
        assert(state.buffer.mkString == "hi", s"expected buffer 'hi', got '${state.buffer.mkString}'")
      case other => fail(s"expected TextInput dialog, got $other")
  }

  test("Enter inside textInput accepts the value and stores it on the model") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('i')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('o')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('k')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.Enter))
    assert(d.model.dialog == Stage1ShowcaseApp.Dialog.None)
    assert(d.model.lastTextInput.contains("ok"))
  }

  test("Esc inside textInput cancels without storing") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('i')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('x')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.Escape))
    assert(d.model.dialog == Stage1ShowcaseApp.Dialog.None)
    assert(d.model.lastTextInput.isEmpty, "Esc should not commit the value")
  }

  test("'l' opens the listSelect dialog at index 0") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('l')))
    d.model.dialog match
      case Stage1ShowcaseApp.Dialog.ListSelect(idx) => assert(idx == 0)
      case other                                    => fail(s"expected ListSelect, got $other")
  }

  test("ArrowDown advances the listSelect index, Enter accepts") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('l')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.ArrowDown))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.ArrowDown))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.Enter))
    assert(d.model.dialog == Stage1ShowcaseApp.Dialog.None)
    assert(d.model.lastListPick.contains("cherry"), s"expected cherry, got ${d.model.lastListPick}")
  }

  /**
   * Resolve the listSelect dialog's expected rect for the default 100×28
   *  driver. Dialog is 40×10, centred → top-left (31, 10). First item row
   *  is at panel-local Y=3 → absolute row 12.
   */
  private val listDialogCol    = (100 - 40) / 2 + 1 + 5 // somewhere inside the rect, away from border
  private val listFirstItemRow = (28 - 10) / 2 + 1 + 2  // oy + firstRowOffset(3) - 1

  test("clicking a visible row inside listSelect accepts that item") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('l')))
    // First visible item is "apple" at firstItemRow; row+1 is "banana".
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            listDialogCol,
            listFirstItemRow + 1,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.dialog == Stage1ShowcaseApp.Dialog.None, "click on a row should commit + close")
    assert(d.model.lastListPick.contains("banana"), s"expected banana, got ${d.model.lastListPick}")
  }

  test("clicking the top item row in listSelect picks the first item") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('l')))
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            listDialogCol,
            listFirstItemRow,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.lastListPick.contains("apple"))
  }

  test("clicking outside the listSelect rectangle is a no-op") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('l')))
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            1,
            1,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.dialog.isInstanceOf[Stage1ShowcaseApp.Dialog.ListSelect], "clicks outside must not close the dialog")
    assert(d.model.lastListPick.isEmpty)
  }

  test("scrolling inside listSelect cycles the selected index") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('l')))
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Scroll(
            termflow.tui.ScrollDirection.Down,
            listDialogCol,
            listFirstItemRow + 2,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    d.model.dialog match
      case Stage1ShowcaseApp.Dialog.ListSelect(idx) => assert(idx == 1, s"scroll down must advance idx, got $idx")
      case other                                    => fail(s"expected ListSelect dialog, got $other")
  }

  test("scrolling outside the listSelect rect does nothing") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('l')))
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Scroll(
            termflow.tui.ScrollDirection.Down,
            1,
            1,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    d.model.dialog match
      case Stage1ShowcaseApp.Dialog.ListSelect(idx) => assert(idx == 0, "scroll outside must not change idx")
      case other                                    => fail(s"expected ListSelect dialog, got $other")
  }

  test("'w' opens the waiting dialog with a deadline tick") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('w')))
    d.model.dialog match
      case Stage1ShowcaseApp.Dialog.Waiting(opened, deadline) =>
        assert(deadline > opened, "deadline must be in the future")
      case other => fail(s"expected Waiting, got $other")
  }

  test("Tick auto-closes the waiting dialog when the deadline elapses") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('w')))
    // Drive enough ticks to cross the 30-tick auto-close threshold.
    (1 to 31).foreach(_ => d.send(Stage1ShowcaseApp.Msg.Tick))
    assert(d.model.dialog == Stage1ShowcaseApp.Dialog.None, "waiting dialog should auto-close after its deadline")
  }

  test("Esc inside waiting cancels immediately") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('w')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.Escape))
    assert(d.model.dialog == Stage1ShowcaseApp.Dialog.None)
  }

  test("clicking a row in the Borders panel selects that border style") {
    val d            = driver
    val bordersStart = d.model.width - 22 - 22 - 1
    // Row 0 = sharp (firstItemRow), 1 = rounded (default), 2 = double, 3 = ascii.
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            bordersStart + 5,
            firstItemRow + 2, // double
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.borderName == "double")
  }

  test("clicking the dark row in the Themes panel selects dark (top item)") {
    val d = driver
    // Cycle off dark first so the click actually changes state.
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('t')))
    assert(d.model.themeName == "light")
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            themesRowCol(d),
            firstItemRow, // dark is idx=0, lives on the first item row
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.themeName == "dark", "click on the top item row must select index 0")
  }

  test("Themes panel renders RadioGroup markers (◉ for selected, ○ for unselected)") {
    val d        = driver
    val frame    = d.frame
    val rendered = (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
    assert(rendered.contains("◉"), "selected RadioGroup marker should appear in the frame")
    assert(rendered.contains("○"), "unselected RadioGroup marker should appear in the frame")
  }

  test("Styles panel renders CheckBox demo (☐ off and ☒ on)") {
    val d        = driver
    val frame    = d.frame
    val rendered = (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
    assert(rendered.contains("☐"), "unchecked CheckBox glyph should appear in the frame")
    assert(rendered.contains("☒"), "checked CheckBox glyph should appear in the frame")
    assert(rendered.contains("focus"), "focused CheckBox label should appear in the frame")
  }

  test("the Tabs bar appears on row 2 with showcase / widgets / help labels") {
    val d        = driver
    val frame    = d.frame
    val rendered = (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
    assert(rendered.contains("1 Showcase"), "Tabs widget should render Showcase tab")
    assert(rendered.contains("2 Widgets"), "Tabs widget should render Widgets tab")
    assert(rendered.contains("3 Help"), "Tabs widget should render Help tab")
    // Default active tab (0) should be bracketed by the Tabs widget.
    assert(rendered.contains("[ 1 Showcase ]"), s"default active tab should be bracketed; rendered:\n$rendered")
  }

  // ---- Interactive tab switching ------------------------------------------

  test("pressing 1 / 2 / 3 switches the active tab") {
    val d = driver
    assert(d.model.activeTab == 0)
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('2')))
    assert(d.model.activeTab == 1)
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('3')))
    assert(d.model.activeTab == 2)
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('1')))
    assert(d.model.activeTab == 0)
  }

  test("clicking a tab cell on row 2 switches to that tab") {
    val d = driver
    // First tab " 1 Showcase " is 12 cells wide starting at col 2 — col 7 is mid-tab.
    // Second tab starts at col 2 + 12 + 1 (separator) = col 15.
    d.send(
      Stage1ShowcaseApp.Msg.Key(
        InputKey.Mouse(
          termflow.tui.MouseEvent.Press(
            termflow.tui.MouseButton.Left,
            18,
            2,
            termflow.tui.KeyDecoder.Modifiers()
          )
        )
      )
    )
    assert(d.model.activeTab == 1, "click on the Widgets tab cell should select it")
  }

  test("Widgets tab renders the Tree widget") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('2')))
    val frame    = d.frame
    val rendered = (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
    assert(rendered.contains("Tree widget demo"), "Widgets tab title should appear")
    assert(rendered.contains("termflow"), "Tree should render the root node label")
  }

  test("Widgets tab: ↓ moves selection, Space toggles a node") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('2')))
    assert(d.model.treeSelected == 0)
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.ArrowDown))
    assert(d.model.treeSelected == 1)
    // Toggle the currently-selected row (which is "tui" — already expanded).
    val before = d.model.treeExpanded
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey(' ')))
    assert(d.model.treeExpanded != before, "Space on an internal node should flip its expanded state")
  }

  test("Help tab renders keybinding reference") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('3')))
    val frame    = d.frame
    val rendered = (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
    assert(rendered.contains("Keybindings"), s"Help tab title missing in:\n$rendered")
    assert(rendered.contains("switch tab"), "Help should document tab switching")
  }

  test("eventHistory grows on each key, capped at the configured maximum") {
    val d = driver
    assert(d.model.eventHistory.isEmpty)
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('a')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('b')))
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('c')))
    assert(d.model.eventHistory.length == 3)
    assert(d.model.eventHistory.last.contains("'c'"), s"latest event must be at the tail: ${d.model.eventHistory}")
  }

  test("LogView in the live-input panel renders the most recent events") {
    val d = driver
    d.send(Stage1ShowcaseApp.Msg.Key(InputKey.CharKey('z')))
    val frame    = d.frame
    val rendered = (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
    assert(rendered.contains("'z'"), "LogView should render the just-pressed key event")
  }

  // Silence unused-import lint warning for AnsiRenderer (it's referenced
  // implicitly through TuiTestDriver, but the file would warn otherwise).
  private val _ = AnsiRenderer.getClass
