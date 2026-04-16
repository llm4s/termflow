package termflow.apps.forms

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.forms.FormDemoApp.*
import termflow.testkit.GoldenSupport
import termflow.testkit.TuiTestDriver
import termflow.tui.KeyDecoder.InputKey

class FormDemoAppSpec extends AnyFunSuite with GoldenSupport:

  private val Width  = 80
  private val Height = 22

  private def driver(): TuiTestDriver[Model, Msg] =
    val d = TuiTestDriver(App, width = Width, height = Height)
    d.init()
    d

  /** Type a string into the focused field one CharKey at a time. */
  private def typeKeys(d: TuiTestDriver[Model, Msg], s: String): Unit =
    s.foreach(c => d.send(Msg.ConsoleInputKey(InputKey.CharKey(c))))

  // --- focus order ---------------------------------------------------------

  test("initial state: Name is focused, no submission yet, dark theme"):
    val d = driver()
    assert(d.model.fm.isFocused(NameId))
    assert(d.model.submitted.isEmpty)
    assert(d.model.darkTheme)

  test("Tab cycles focus through Name → Email → Bio → Submit → Reset → Name"):
    val d        = driver()
    val expected = Vector(EmailId, BioId, SubmitId, ResetId, NameId)
    expected.foreach { id =>
      d.send(Msg.NextFocus)
      assert(d.model.fm.current.contains(id), s"expected focus $id, got ${d.model.fm.current}")
    }

  test("BackTab (Shift+Tab) walks focus backward through the cycle"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(InputKey.BackTab))
    assert(d.model.fm.isFocused(ResetId))
    d.send(Msg.ConsoleInputKey(InputKey.BackTab))
    assert(d.model.fm.isFocused(SubmitId))
    d.send(Msg.ConsoleInputKey(InputKey.BackTab))
    assert(d.model.fm.isFocused(BioId))

  // --- arrow-key focus navigation ------------------------------------------

  test("ArrowDown advances focus from inside a TextField (vertical arrows are global)"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.ConsoleInputKey(InputKey.ArrowDown))
    assert(d.model.fm.isFocused(EmailId))
    assert(d.model.name.buffer == "alice", "ArrowDown must not damage the field buffer")

  test("ArrowUp on a TextField walks back through the cycle"):
    val d = driver()
    d.send(Msg.NextFocus) // Email
    d.send(Msg.NextFocus) // Bio
    d.send(Msg.ConsoleInputKey(InputKey.ArrowUp))
    assert(d.model.fm.isFocused(EmailId))

  test("ArrowDown from the last element wraps back to Name"):
    val d = driver()
    (1 to 4).foreach(_ => d.send(Msg.NextFocus)) // -> Reset
    d.send(Msg.ConsoleInputKey(InputKey.ArrowDown))
    assert(d.model.fm.isFocused(NameId))

  test("ArrowUp from Name wraps to Reset"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(InputKey.ArrowUp))
    assert(d.model.fm.isFocused(ResetId))

  test("ArrowRight / ArrowLeft on a button advances / retreats focus"):
    val d = driver()
    (1 to 3).foreach(_ => d.send(Msg.NextFocus)) // -> Submit
    d.send(Msg.ConsoleInputKey(InputKey.ArrowRight))
    assert(d.model.fm.isFocused(ResetId))
    d.send(Msg.ConsoleInputKey(InputKey.ArrowLeft))
    assert(d.model.fm.isFocused(SubmitId))
    d.send(Msg.ConsoleInputKey(InputKey.ArrowLeft))
    assert(d.model.fm.isFocused(BioId)) // linear cycle: Submit <- Bio

  test("ArrowLeft inside a TextField moves the in-field cursor, NOT focus"):
    val d = driver()
    typeKeys(d, "abc")
    assert(d.model.name.cursor == 3)
    d.send(Msg.ConsoleInputKey(InputKey.ArrowLeft))
    assert(d.model.fm.isFocused(NameId), "focus must stay on Name")
    assert(d.model.name.cursor == 2, "cursor should have moved within the buffer")

  test("ArrowRight inside a TextField moves the in-field cursor, NOT focus"):
    val d = driver()
    typeKeys(d, "abc")
    // Move to index 1, then press right and expect index 2.
    d.send(Msg.ConsoleInputKey(InputKey.Home))
    d.send(Msg.ConsoleInputKey(InputKey.ArrowRight))
    assert(d.model.fm.isFocused(NameId))
    assert(d.model.name.cursor == 1)

  test("Globals bind vertical arrows but NOT horizontal arrows"):
    val g = FormDemoApp.Globals
    assert(g.lookup(InputKey.ArrowUp).contains(Msg.PrevFocus))
    assert(g.lookup(InputKey.ArrowDown).contains(Msg.NextFocus))
    // Left/Right must stay out of Globals so they can reach TextField.
    assert(g.lookup(InputKey.ArrowLeft).isEmpty)
    assert(g.lookup(InputKey.ArrowRight).isEmpty)

  test("NonTextShortcuts bind horizontal arrows for button-focus routing"):
    val n = FormDemoApp.NonTextShortcuts
    assert(n.lookup(InputKey.ArrowLeft).contains(Msg.PrevFocus))
    assert(n.lookup(InputKey.ArrowRight).contains(Msg.NextFocus))

  // --- TextField key routing -----------------------------------------------

  test("typing into the focused field updates only that field's buffer"):
    val d = driver()
    typeKeys(d, "alice")
    assert(d.model.name.buffer == "alice")
    assert(d.model.email.buffer == "")
    assert(d.model.bio.buffer == "")

  test("Tab inside a TextField advances focus and keeps the buffer"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.ConsoleInputKey(InputKey.Ctrl('I'))) // Tab
    assert(d.model.fm.isFocused(EmailId))
    assert(d.model.name.buffer == "alice")

  test("'t' inside a TextField is a printable character, not a theme toggle"):
    val d      = driver()
    val before = d.model.darkTheme
    typeKeys(d, "tomato")
    assert(d.model.name.buffer == "tomato", "'t' and 'T' must reach the focused field's buffer")
    assert(d.model.darkTheme == before, "theme must not have toggled from plain letter input")

  test("'q' inside a TextField is a printable character, not a quit command"):
    val d = driver()
    typeKeys(d, "quinn")
    assert(d.model.name.buffer == "quinn")
    assert(!d.exited)

  // --- Enter semantics inside fields ---------------------------------------

  test("Enter inside a TextField submits the form (requested behaviour)"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.submitted.exists(_.name == "alice"), "Enter inside any field should submit")
    // Buffer is preserved so the form stays editable for corrections.
    assert(d.model.name.buffer == "alice")

  test("Enter inside a mid-form field still submits with current partial values"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.NextFocus) // -> Email
    typeKeys(d, "a@b.c")
    d.send(Msg.ConsoleInputKey(InputKey.Enter)) // while still in Email
    val s = d.model.submitted.get
    assert(s.name == "alice")
    assert(s.email == "a@b.c")
    assert(s.bio == "") // Bio never typed

  // --- Submit / Reset buttons ----------------------------------------------

  test("Enter on the Submit button captures the current field values"):
    val d = driver()
    (1 to 3).foreach(_ => d.send(Msg.NextFocus)) // skip fields -> Submit
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.submitted.contains(Submission("", "", "")))

  test("Space on the Submit button also activates"):
    val d = driver()
    typeKeys(d, "x")
    (1 to 3).foreach(_ => d.send(Msg.NextFocus)) // -> Submit
    d.send(Msg.ConsoleInputKey(InputKey.CharKey(' ')))
    assert(d.model.submitted.exists(_.name == "x"))

  test("Enter on the Reset button clears all fields and the submission"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.ConsoleInputKey(InputKey.Enter)) // submit from inside Name
    assert(d.model.submitted.isDefined)
    (1 to 4).foreach(_ => d.send(Msg.NextFocus)) // -> Reset
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.name.buffer == "")
    assert(d.model.email.buffer == "")
    assert(d.model.bio.buffer == "")
    assert(d.model.submitted.isEmpty)

  // --- global bindings (true globals vs non-text shortcuts) ----------------

  test("Ctrl+T toggles the theme from anywhere, including inside a TextField"):
    val d      = driver()
    val before = d.model.darkTheme
    d.send(Msg.ConsoleInputKey(InputKey.Ctrl('T')))
    assert(d.model.darkTheme == !before)

  test("'t' on a button-focused row toggles the theme (non-text shortcut)"):
    val d = driver()
    (1 to 3).foreach(_ => d.send(Msg.NextFocus)) // -> Submit
    val before = d.model.darkTheme
    d.send(Msg.ConsoleInputKey(InputKey.CharKey('t')))
    assert(d.model.darkTheme == !before)

  test("'q' on a button-focused row quits (non-text shortcut)"):
    val d = driver()
    (1 to 3).foreach(_ => d.send(Msg.NextFocus)) // -> Submit
    d.send(Msg.ConsoleInputKey(InputKey.CharKey('q')))
    assert(d.exited)

  test("Ctrl+C quits from anywhere"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(InputKey.Ctrl('C')))
    assert(d.exited)

  test("Esc quits from anywhere"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(InputKey.Escape))
    assert(d.exited)

  // --- Keymap wiring -------------------------------------------------------

  test("Globals bind only truly-global keys — no printable letters"):
    val g = FormDemoApp.Globals
    assert(g.lookup(InputKey.Ctrl('I')).contains(Msg.NextFocus)) // Tab
    assert(g.lookup(InputKey.BackTab).contains(Msg.PrevFocus))   // Shift+Tab
    assert(g.lookup(InputKey.Ctrl('C')).contains(Msg.Quit))
    assert(g.lookup(InputKey.Escape).contains(Msg.Quit))
    assert(g.lookup(InputKey.Ctrl('T')).contains(Msg.ToggleTheme))
    // Critically, letters are NOT in Globals so they can reach text fields.
    assert(g.lookup(InputKey.CharKey('t')).isEmpty)
    assert(g.lookup(InputKey.CharKey('q')).isEmpty)

  test("NonTextShortcuts hold the letter bindings that only fire outside fields"):
    val n = FormDemoApp.NonTextShortcuts
    assert(n.lookup(InputKey.CharKey('t')).contains(Msg.ToggleTheme))
    assert(n.lookup(InputKey.CharKey('T')).contains(Msg.ToggleTheme))
    assert(n.lookup(InputKey.CharKey('q')).contains(Msg.Quit))
    assert(n.lookup(InputKey.CharKey('Q')).contains(Msg.Quit))

  // --- frame structure -----------------------------------------------------

  test("only the focused TextField row paints an inverse-video cursor cell"):
    val d     = driver()
    val frame = d.frame
    val nameRowIdx = (0 until frame.height).indexWhere { r =>
      (0 until frame.width).map(c => frame.cells(r)(c).ch).mkString.contains("Name:")
    }
    assert(nameRowIdx >= 0)
    val row         = frame.cells(nameRowIdx)
    val cursorCells = (0 until frame.width).count(c => row(c).style.bg == termflow.tui.Theme.dark.primary)
    assert(cursorCells == 1, s"expected exactly one inverse cell on the focused row; saw $cursorCells")

  test("after Tab, the cursor block moves from Name row to Email row"):
    val d = driver()
    d.send(Msg.NextFocus)
    val frame = d.frame
    val emailRowIdx = (0 until frame.height).indexWhere { r =>
      (0 until frame.width).map(c => frame.cells(r)(c).ch).mkString.contains("Email:")
    }
    val nameRowIdx = (0 until frame.height).indexWhere { r =>
      (0 until frame.width).map(c => frame.cells(r)(c).ch).mkString.contains("Name:")
    }
    val emailHasCursor =
      (0 until frame.width).exists(c => frame.cells(emailRowIdx)(c).style.bg == termflow.tui.Theme.dark.primary)
    val nameHasCursor =
      (0 until frame.width).exists(c => frame.cells(nameRowIdx)(c).style.bg == termflow.tui.Theme.dark.primary)
    assert(emailHasCursor, "expected cursor on Email row")
    assert(!nameHasCursor, "expected no cursor on Name row")

  test("bottom status bar paints theme.primary bg so it's visible on any terminal"):
    // Pins the fix for the 'theme invisible on black monitor' bug: the
    // StatusBar at the last row uses inverse video (theme.primary as bg),
    // which is always painted and therefore always visible, regardless of
    // what colour the user's terminal happens to be configured with.
    val d            = driver()
    val frame        = d.frame
    val last         = frame.cells(frame.height - 1)
    val inverseCells = (0 until frame.width).count(c => last(c).style.bg == termflow.tui.Theme.dark.primary)
    assert(inverseCells > 0, "expected the bottom status bar to paint theme.primary as bg")
    // Spot-check it contains the documented hint.
    val row = (0 until frame.width).map(c => last(c).ch).mkString
    assert(row.contains("theme="))
    assert(row.contains("Ctrl+T"))

  // --- golden snapshots (deterministic thanks to #92) -----------------------

  test("initial frame matches the dark-theme golden"):
    val d = driver()
    assertGoldenFrame(d.frame, "initial-dark")

  test("after typing into Name + Tab to Email"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.ConsoleInputKey(InputKey.Ctrl('I'))) // Tab
    assertGoldenFrame(d.frame, "name-typed-email-focused")

  test("after submitting a complete form"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.NextFocus)
    typeKeys(d, "a@b.c")
    d.send(Msg.NextFocus)
    typeKeys(d, "engineer")
    d.send(Msg.ConsoleInputKey(InputKey.Enter)) // Enter from Bio submits
    assertGoldenFrame(d.frame, "submitted")
