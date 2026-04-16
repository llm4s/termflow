package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.KeyDecoder.InputKey

class TextFieldSpec extends AnyFunSuite:

  given Theme = Theme.dark

  // --- State construction ---------------------------------------------------

  test("State.empty has empty buffer, cursor 0, no placeholder"):
    val s = TextField.State.empty
    assert(s.buffer == "")
    assert(s.cursor == 0)
    assert(s.placeholder == "")
    assert(s.isEmpty)
    assert(s.length == 0)

  test("State.withPlaceholder sets a hint without filling the buffer"):
    val s = TextField.State.withPlaceholder("name")
    assert(s.buffer == "")
    assert(s.placeholder == "name")
    assert(s.isEmpty)

  test("State.of pre-populates and parks the cursor at end"):
    val s = TextField.State.of("hello")
    assert(s.buffer == "hello")
    assert(s.cursor == 5)
    assert(!s.isEmpty)
    assert(s.length == 5)

  // --- handleKey: insertion -------------------------------------------------

  private def submitOpt: String => Option[String] = _ => None

  test("CharKey inserts at the cursor and advances it"):
    val (s1, _) = TextField.handleKey(TextField.State.empty, InputKey.CharKey('a'))(submitOpt)
    val (s2, _) = TextField.handleKey(s1, InputKey.CharKey('b'))(submitOpt)
    val (s3, _) = TextField.handleKey(s2, InputKey.CharKey('c'))(submitOpt)
    assert(s3.buffer == "abc")
    assert(s3.cursor == 3)

  test("CharKey inserts at the cursor position when not at end"):
    val s0      = TextField.State.of("ac")
    val s1      = s0.copy(cursor = 1) // between 'a' and 'c'
    val (s2, _) = TextField.handleKey(s1, InputKey.CharKey('b'))(submitOpt)
    assert(s2.buffer == "abc")
    assert(s2.cursor == 2)

  // --- handleKey: deletion --------------------------------------------------

  test("Backspace removes the char before the cursor"):
    val s0      = TextField.State.of("abc")
    val (s1, _) = TextField.handleKey(s0, InputKey.Backspace)(submitOpt)
    assert(s1.buffer == "ab")
    assert(s1.cursor == 2)

  test("Backspace at cursor 0 is a no-op"):
    val s0      = TextField.State.of("abc").copy(cursor = 0)
    val (s1, _) = TextField.handleKey(s0, InputKey.Backspace)(submitOpt)
    assert(s1.buffer == "abc")
    assert(s1.cursor == 0)

  test("Delete removes the char at the cursor"):
    val s0      = TextField.State.of("abc").copy(cursor = 1) // between 'a' and 'b'
    val (s1, _) = TextField.handleKey(s0, InputKey.Delete)(submitOpt)
    assert(s1.buffer == "ac")
    assert(s1.cursor == 1)

  test("Delete at end-of-buffer is a no-op"):
    val s0      = TextField.State.of("abc")
    val (s1, _) = TextField.handleKey(s0, InputKey.Delete)(submitOpt)
    assert(s1.buffer == "abc")
    assert(s1.cursor == 3)

  // --- handleKey: cursor motion ---------------------------------------------

  test("ArrowLeft / ArrowRight move the cursor and clamp at the bounds"):
    val s0      = TextField.State.of("abc")
    val (s1, _) = TextField.handleKey(s0, InputKey.ArrowLeft)(submitOpt)
    assert(s1.cursor == 2)
    val (s2, _) = TextField.handleKey(s1, InputKey.ArrowLeft)(submitOpt)
    val (s3, _) = TextField.handleKey(s2, InputKey.ArrowLeft)(submitOpt)
    val (s4, _) = TextField.handleKey(s3, InputKey.ArrowLeft)(submitOpt)
    assert(s4.cursor == 0) // clamped, no underflow
    val (s5, _) = TextField.handleKey(s4, InputKey.ArrowRight)(submitOpt)
    assert(s5.cursor == 1)
    // Walk past end → clamp at length
    val (sEnd, _) = (1 to 10).foldLeft((s5, Option.empty[String])) { case ((acc, _), _) =>
      TextField.handleKey(acc, InputKey.ArrowRight)(submitOpt)
    }
    assert(sEnd.cursor == 3)

  test("Home moves cursor to 0; End moves cursor to length"):
    val s0      = TextField.State.of("abc").copy(cursor = 2)
    val (s1, _) = TextField.handleKey(s0, InputKey.Home)(submitOpt)
    assert(s1.cursor == 0)
    val (s2, _) = TextField.handleKey(s1, InputKey.End)(submitOpt)
    assert(s2.cursor == 3)

  // --- handleKey: submit ----------------------------------------------------

  test("Enter calls onSubmit with the buffer and clears the field"):
    val s0      = TextField.State.of("ready")
    val (s1, m) = TextField.handleKey(s0, InputKey.Enter)(b => Some(s"submitted:$b"))
    assert(s1.buffer == "")
    assert(s1.cursor == 0)
    assert(m.contains("submitted:ready"))

  test("Enter still clears the buffer even when onSubmit returns None"):
    val s0      = TextField.State.of("nope")
    val (s1, m) = TextField.handleKey(s0, InputKey.Enter)(_ => None)
    assert(s1.buffer == "")
    assert(s1.cursor == 0)
    assert(m.isEmpty)

  test("global keys (Ctrl+C, Ctrl+D) are NOT handled — pass through unchanged"):
    val s0       = TextField.State.of("abc")
    val (s1, m1) = TextField.handleKey(s0, InputKey.Ctrl('C'))(submitOpt)
    val (s2, m2) = TextField.handleKey(s0, InputKey.Ctrl('D'))(submitOpt)
    assert(s1 == s0 && m1.isEmpty)
    assert(s2 == s0 && m2.isEmpty)

  // --- view rendering -------------------------------------------------------

  private def render(v: VNode, width: Int): (Array[Char], Array[Style]) =
    val frame = AnsiRenderer.buildFrame(RootNode(width, 1, List(v), None))
    val chars = (0 until width).map(c => frame.cells(0)(c).ch).toArray
    val style = (0 until width).map(c => frame.cells(0)(c).style).toArray
    (chars, style)

  test("unfocused render with empty buffer + placeholder shows the placeholder"):
    val s       = TextField.State.withPlaceholder("name…")
    val v       = TextField.view(s, lineWidth = 10)
    val (cs, _) = render(v, 10)
    assert(cs.mkString == "name…     ")

  test("unfocused render with empty buffer + no placeholder shows blanks"):
    val v       = TextField.view(TextField.State.empty, lineWidth = 5)
    val (cs, _) = render(v, 5)
    assert(cs.mkString == "     ")

  test("unfocused render of populated buffer shows the buffer left-aligned"):
    val v       = TextField.view(TextField.State.of("hi"), lineWidth = 6)
    val (cs, _) = render(v, 6)
    assert(cs.mkString == "hi    ")

  test("focused render shows an inverse-video cursor cell"):
    val s            = TextField.State.of("hi") // cursor at 2 (past end)
    val v            = TextField.view(s, lineWidth = 5, focused = true)
    val (cs, styles) = render(v, 5)
    assert(cs.mkString == "hi   ")
    // Cell 0 ('h'): theme.primary fg, no bg
    assert(styles(0).fg == Theme.dark.primary)
    assert(styles(0).bg == Color.Default)
    // Cell 2 (cursor on space past end): inverse video.
    assert(styles(2).fg == Theme.dark.background)
    assert(styles(2).bg == Theme.dark.primary)
    // Cells past cursor: normal style.
    assert(styles(3).bg == Color.Default)

  test("focused render with cursor in the middle paints the right cell"):
    val s            = TextField.State.of("abc").copy(cursor = 1) // between 'a' and 'b'
    val v            = TextField.view(s, lineWidth = 4, focused = true)
    val (cs, styles) = render(v, 4)
    assert(cs.mkString == "abc ")
    // Cursor is on 'b' — that cell should be inverse-video.
    assert(styles(1).fg == Theme.dark.background)
    assert(styles(1).bg == Theme.dark.primary)
    // 'a' and 'c' are plain.
    assert(styles(0).bg == Color.Default)
    assert(styles(2).bg == Color.Default)

  test("focused render hides the placeholder"):
    val s            = TextField.State.withPlaceholder("name")
    val v            = TextField.view(s, lineWidth = 6, focused = true)
    val (cs, styles) = render(v, 6)
    // Placeholder must not appear; field is blank with cursor at col 0.
    assert(cs.mkString == "      ")
    assert(styles(0).bg == Theme.dark.primary) // cursor inverse-video on first cell

  test("focused render clamps the cursor to the rightmost cell when buffer overflows"):
    val s            = TextField.State.of("abcdefgh") // cursor at 8
    val v            = TextField.view(s, lineWidth = 4, focused = true)
    val (cs, styles) = render(v, 4)
    // Truncated to "abcd"; cursor visually pinned at the last cell.
    assert(cs.mkString == "abcd")
    assert(styles(3).bg == Theme.dark.primary)

  test("TextField.width reports lineWidth (clamped to >= 1)"):
    assert(TextField.width(20) == 20)
    assert(TextField.width(0) == 1)
    assert(TextField.width(-3) == 1)

  test("TextField composes inside Layout.column with predictable measure"):
    val a = TextField.view(TextField.State.of("alice"), lineWidth = 10)
    val b = TextField.view(TextField.State.of("bob"), lineWidth = 10)
    val c = Layout.column(gap = 1)(a, b)
    assert(c.measure == (10, 3)) // 10 wide, two rows + 1 gap
