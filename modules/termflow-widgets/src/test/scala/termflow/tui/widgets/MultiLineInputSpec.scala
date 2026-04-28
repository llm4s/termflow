package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.KeyDecoder.InputKey

class MultiLineInputSpec extends AnyFunSuite:

  given Theme = Theme.dark

  // ---- State.of -----------------------------------------------------------

  test("State.of seeds empty editor with one empty line"):
    val s = MultiLineInput.State.of("")
    assert(s.lines == Vector(""))
    assert(s.cursorRow == 0 && s.cursorCol == 0)

  test("State.of splits text on newlines and parks cursor at the end"):
    val s = MultiLineInput.State.of("a\nbb\nccc")
    assert(s.lines == Vector("a", "bb", "ccc"))
    assert(s.cursorRow == 2)
    assert(s.cursorCol == 3)

  // ---- CharKey ------------------------------------------------------------

  test("CharKey inserts at the cursor and advances col"):
    val (s, _) = MultiLineInput.handleKey[String](MultiLineInput.State.empty, InputKey.CharKey('a'))
    assert(s.lines == Vector("a"))
    assert(s.cursorCol == 1)

  // ---- Enter --------------------------------------------------------------

  test("Enter splits the current line at the cursor"):
    val initial = MultiLineInput.State.of("hello").copy(cursorCol = 2)
    val (s, _)  = MultiLineInput.handleKey[String](initial, InputKey.Enter)
    assert(s.lines == Vector("he", "llo"))
    assert(s.cursorRow == 1 && s.cursorCol == 0)

  // ---- Backspace ----------------------------------------------------------

  test("Backspace at start of a line joins with the previous line"):
    val initial = MultiLineInput.State(lines = Vector("foo", "bar"), cursorRow = 1, cursorCol = 0)
    val (s, _)  = MultiLineInput.handleKey[String](initial, InputKey.Backspace)
    assert(s.lines == Vector("foobar"))
    assert(s.cursorRow == 0 && s.cursorCol == 3)

  test("Backspace inside a line deletes a grapheme"):
    val initial = MultiLineInput.State.of("a😀b").copy(cursorCol = 3) // after the emoji
    val (s, _)  = MultiLineInput.handleKey[String](initial, InputKey.Backspace)
    assert(s.lines == Vector("ab"))
    assert(s.cursorCol == 1)

  // ---- Delete -------------------------------------------------------------

  test("Delete at end of a line joins with the following line"):
    val initial = MultiLineInput.State(lines = Vector("foo", "bar"), cursorRow = 0, cursorCol = 3)
    val (s, _)  = MultiLineInput.handleKey[String](initial, InputKey.Delete)
    assert(s.lines == Vector("foobar"))
    assert(s.cursorRow == 0 && s.cursorCol == 3)

  // ---- Arrows -------------------------------------------------------------

  test("ArrowUp/ArrowDown moves the cursor between rows, clamping col"):
    val initial = MultiLineInput.State(lines = Vector("ab", "longer"), cursorRow = 1, cursorCol = 5)
    val (up, _) = MultiLineInput.handleKey[String](initial, InputKey.ArrowUp)
    assert(up.cursorRow == 0)
    assert(up.cursorCol == 2, "col clamps to the shorter row")
    val (down, _) = MultiLineInput.handleKey[String](up, InputKey.ArrowDown)
    assert(down.cursorRow == 1)
    // Clamping doesn't restore the original column.
    assert(down.cursorCol == 2)

  test("ArrowLeft at start of a line moves to the end of the previous line"):
    val initial = MultiLineInput.State(lines = Vector("ab", "cd"), cursorRow = 1, cursorCol = 0)
    val (s, _)  = MultiLineInput.handleKey[String](initial, InputKey.ArrowLeft)
    assert(s.cursorRow == 0 && s.cursorCol == 2)

  test("ArrowRight at end of a line moves to start of the next line"):
    val initial = MultiLineInput.State(lines = Vector("ab", "cd"), cursorRow = 0, cursorCol = 2)
    val (s, _)  = MultiLineInput.handleKey[String](initial, InputKey.ArrowRight)
    assert(s.cursorRow == 1 && s.cursorCol == 0)

  test("ArrowLeft steps over a surrogate pair"):
    val initial = MultiLineInput.State.of("a😀b").copy(cursorCol = 3)
    val (s, _)  = MultiLineInput.handleKey[String](initial, InputKey.ArrowLeft)
    assert(s.cursorCol == 1, "should jump back over the surrogate pair")

  // ---- Home / End ---------------------------------------------------------

  test("Home moves to col 0; End moves to end of line"):
    val initial = MultiLineInput.State.of("hello").copy(cursorCol = 2)
    val (h, _)  = MultiLineInput.handleKey[String](initial, InputKey.Home)
    val (e, _)  = MultiLineInput.handleKey[String](initial, InputKey.End)
    assert(h.cursorCol == 0)
    assert(e.cursorCol == 5)

  // ---- render -------------------------------------------------------------

  test("render emits one node per visible row"):
    val s     = MultiLineInput.State(lines = Vector("a", "b", "c"), cursorRow = 0, cursorCol = 0)
    val nodes = MultiLineInput.render(s, width = 10, height = 3)
    assert(nodes.size == 3)
    // Every visible row is a TextNode — the cursor row uses a
    // reverse-video cell rather than commandeering the hardware cursor.
    assert(nodes.forall(_.isInstanceOf[TextNode]))

  test("render scrolls so the cursor row stays visible"):
    val lines = (0 until 10).map(i => s"line-$i").toVector
    val s     = MultiLineInput.State(lines = lines, cursorRow = 9, cursorCol = 0)
    val nodes = MultiLineInput.render(s, width = 20, height = 3)
    assert(nodes.size == 3)
    // The cursor row (#9) is the last visible row. Concatenate text
    // segments to recover its rendered string.
    val texts = nodes.map { case t: TextNode => t.txt.map(_.txt).mkString; case _ => "" }
    assert(texts.last.startsWith("line-9"))

  test("render returns Nil for non-positive size"):
    val s = MultiLineInput.State.empty
    assert(MultiLineInput.render(s, width = 0, height = 5).isEmpty)
    assert(MultiLineInput.render(s, width = 5, height = 0).isEmpty)
