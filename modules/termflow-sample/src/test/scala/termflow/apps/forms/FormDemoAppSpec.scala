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

  test("Shift+Tab equivalent (PrevFocus) walks the cycle backwards"):
    val d = driver()
    d.send(Msg.PrevFocus)
    assert(d.model.fm.isFocused(ResetId))
    d.send(Msg.PrevFocus)
    assert(d.model.fm.isFocused(SubmitId))

  // --- TextField key routing -----------------------------------------------

  test("typing into the focused field updates only that field's buffer"):
    val d = driver()
    typeKeys(d, "alice")
    assert(d.model.name.buffer == "alice")
    assert(d.model.email.buffer == "")
    assert(d.model.bio.buffer == "")

  test("Enter inside a TextField advances focus AND keeps the buffer"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.fm.isFocused(EmailId))
    assert(d.model.name.buffer == "alice", "Enter must not clear the field for form usage")

  test("Tab inside a TextField also advances focus and keeps buffer"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.ConsoleInputKey(InputKey.Ctrl('I'))) // Tab arrives as Ctrl+I
    assert(d.model.fm.isFocused(EmailId))
    assert(d.model.name.buffer == "alice")

  // --- Submit / Reset -------------------------------------------------------

  test("Enter on the Submit button captures the current field values"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.NextFocus) // -> Email
    typeKeys(d, "alice@example.com")
    d.send(Msg.NextFocus) // -> Bio
    typeKeys(d, "scala dev")
    d.send(Msg.NextFocus) // -> Submit
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.submitted.contains(Submission("alice", "alice@example.com", "scala dev")))

  test("Space on the Submit button also activates"):
    val d = driver()
    typeKeys(d, "x")
    (1 to 3).foreach(_ => d.send(Msg.NextFocus)) // -> Submit
    d.send(Msg.ConsoleInputKey(InputKey.CharKey(' ')))
    assert(d.model.submitted.exists(_.name == "x"))

  test("Enter on the Reset button clears all fields and the submission"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.NextFocus) // Email
    typeKeys(d, "a@b.c")
    (1 to 2).foreach(_ => d.send(Msg.NextFocus)) // Submit
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.submitted.isDefined)
    d.send(Msg.NextFocus) // Reset
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.name.buffer == "")
    assert(d.model.email.buffer == "")
    assert(d.model.bio.buffer == "")
    assert(d.model.submitted.isEmpty)

  // --- global bindings ------------------------------------------------------

  test("'t' anywhere toggles the theme"):
    val d      = driver()
    val before = d.model.darkTheme
    d.send(Msg.ConsoleInputKey(InputKey.CharKey('t')))
    assert(d.model.darkTheme == !before)

  test("'q' anywhere quits"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(InputKey.CharKey('q')))
    assert(d.exited)

  test("the Keymap binds the documented shortcuts"):
    val k = FormDemoApp.Keys
    assert(k.lookup(InputKey.Ctrl('I')).contains(Msg.NextFocus))
    assert(k.lookup(InputKey.CharKey('t')).contains(Msg.ToggleTheme))
    assert(k.lookup(InputKey.CharKey('q')).contains(Msg.Quit))
    assert(k.lookup(InputKey.Ctrl('C')).contains(Msg.Quit))
    assert(k.lookup(InputKey.Escape).contains(Msg.Quit))

  // --- frame structure -----------------------------------------------------

  test("only the focused TextField paints an inverse-video cursor cell"):
    val d     = driver()
    val frame = d.frame
    // Locate the row containing 'N' of "Name:" and check the cell at the
    // start of that field is in inverse video (theme.primary background).
    val nameRowIdx = (0 until frame.height).indexWhere { r =>
      (0 until frame.width).map(c => frame.cells(r)(c).ch).mkString.contains("Name:")
    }
    assert(nameRowIdx >= 0)
    val row = frame.cells(nameRowIdx)
    // Find first cell of the field area (after the "Name:" label + gap) by
    // looking for the cell where bg is set to theme.primary — that's the
    // inverse cursor.
    val cursorCells = (0 until frame.width).count(c => row(c).style.bg == termflow.tui.Theme.dark.primary)
    assert(cursorCells == 1, s"expected exactly one inverse cell on the focused row; saw $cursorCells")

  test("after Tab, the cursor block moves to the next row"):
    val d = driver()
    d.send(Msg.NextFocus) // -> Email
    val frame = d.frame
    // Find the row containing "Email:" — it should be the one with the
    // inverse cursor now.
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

  // --- golden snapshots (deterministic thanks to #92) -----------------------

  test("initial frame matches the dark-theme golden"):
    val d = driver()
    assertGoldenFrame(d.frame, "initial-dark")

  test("after typing into Name + advancing focus to Email"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assertGoldenFrame(d.frame, "name-typed-email-focused")

  test("after submitting a complete form"):
    val d = driver()
    typeKeys(d, "alice")
    d.send(Msg.NextFocus)
    typeKeys(d, "a@b.c")
    d.send(Msg.NextFocus)
    typeKeys(d, "engineer")
    d.send(Msg.NextFocus) // Submit
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assertGoldenFrame(d.frame, "submitted")
