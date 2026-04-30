package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class LogViewSpec extends AnyFunSuite:

  given Theme = Theme.dark

  // ---- wrapLine -----------------------------------------------------------

  test("wrapLine returns one chunk for short input") {
    assert(LogView.wrapLine("hello", width = 10) == Vector("hello"))
  }

  test("wrapLine breaks at exact column boundaries") {
    assert(LogView.wrapLine("abcdef", width = 2) == Vector("ab", "cd", "ef"))
  }

  test("wrapLine of an empty line yields one empty chunk") {
    assert(LogView.wrapLine("", width = 10) == Vector(""))
  }

  test("wrapLine clamps width to a minimum of 1") {
    assert(LogView.wrapLine("abc", width = 0) == Vector("a", "b", "c"))
  }

  // ---- truncateLine -------------------------------------------------------

  test("truncateLine returns the input when it fits") {
    assert(LogView.truncateLine("hi", width = 5) == "hi")
  }

  test("truncateLine appends an ellipsis when it doesn't fit") {
    assert(LogView.truncateLine("abcdef", width = 4) == "abc…")
  }

  test("truncateLine handles width = 1 with the ellipsis alone") {
    assert(LogView.truncateLine("abcdef", width = 1) == "…")
  }

  // ---- expand -------------------------------------------------------------

  test("expand wraps long lines into multiple display rows") {
    val out = LogView.expand(Seq("abcde", "fg"), width = 2, wrap = true)
    assert(out == Vector("ab", "cd", "e", "fg"))
  }

  test("expand truncates instead of wrapping when wrap = false") {
    val out = LogView.expand(Seq("hello world"), width = 5, wrap = false)
    assert(out == Vector("hell…"))
  }

  // ---- viewport -----------------------------------------------------------

  test("viewport returns the last `height` lines when scrollOffset is 0") {
    val display = (1 to 5).map(_.toString).toVector
    assert(LogView.viewport(display, height = 3, scrollOffset = 0) == Vector("3", "4", "5"))
  }

  test("viewport scrolls up by scrollOffset display lines") {
    val display = (1 to 5).map(_.toString).toVector
    assert(LogView.viewport(display, height = 3, scrollOffset = 1) == Vector("2", "3", "4"))
    assert(LogView.viewport(display, height = 3, scrollOffset = 2) == Vector("1", "2", "3"))
  }

  test("viewport clamps scrollOffset at the top of the buffer") {
    val display = (1 to 5).map(_.toString).toVector
    assert(LogView.viewport(display, height = 3, scrollOffset = 999) == Vector("1", "2", "3"))
    assert(LogView.viewport(display, height = 3, scrollOffset = -10) == Vector("3", "4", "5"))
  }

  test("viewport pads short buffers with leading blank rows") {
    val display = Vector("only-line")
    assert(LogView.viewport(display, height = 3, scrollOffset = 0) == Vector("", "", "only-line"))
  }

  test("viewport with height = 0 returns empty") {
    assert(LogView.viewport(Vector("a"), height = 0, scrollOffset = 0).isEmpty)
  }

  // ---- maxScroll ----------------------------------------------------------

  test("maxScroll is 0 when the buffer fits in the viewport") {
    assert(LogView.maxScroll(Seq("a", "b"), width = 10, height = 5, wrap = true) == 0)
  }

  test("maxScroll is total - height when the buffer overflows") {
    // 10 raw lines @ width 10 wrap = 10 display lines; height 4 → max = 6.
    val lines = (1 to 10).map(i => s"l$i").toSeq
    assert(LogView.maxScroll(lines, width = 10, height = 4, wrap = true) == 6)
  }

  // ---- apply --------------------------------------------------------------

  test("apply renders one TextNode per row, positioned vertically") {
    val nodes = LogView(
      lines = Seq("one", "two", "three"),
      width = 10,
      height = 3,
      at = Coord(2.x, 4.y)
    )
    assert(nodes.length == 3)
    val ys = nodes.map { case TextNode(_, y, _) => y.value; case _ => fail() }
    assert(ys == List(4, 5, 6))
  }

  test("apply pads the top of the viewport when the buffer is shorter than height") {
    val nodes = LogView(lines = Seq("only"), width = 10, height = 3)
    val texts = nodes.map { case TextNode(_, _, runs) => runs.map(_.txt).mkString; case _ => fail() }
    assert(texts == List("", "", "only"))
  }

  test("apply with height = 0 produces no nodes") {
    assert(LogView(lines = Seq("a"), width = 10, height = 0).isEmpty)
  }

  test("apply respects the supplied row style override") {
    val custom = Style(fg = Color.Red, bold = true)
    val nodes  = LogView(Seq("hello"), width = 10, height = 1, style = Some(custom))
    nodes.head match
      case TextNode(_, _, runs) =>
        assert(runs.head.style == custom)
      case _ => fail()
  }

  // ---- Viewport / scrollDelta --------------------------------------------

  private val Viewport = LogView.Viewport(at = Coord(XCoord(2), YCoord(4)), width = 10, height = 5)
  private val NoMods   = KeyDecoder.Modifiers.none

  test("Viewport.contains is true on every cell of the rectangle") {
    assert(Viewport.contains(2, 4))   // top-left
    assert(Viewport.contains(11, 8))  // bottom-right (inclusive)
    assert(!Viewport.contains(1, 4))  // one column to the left
    assert(!Viewport.contains(12, 4)) // one column past the right
    assert(!Viewport.contains(2, 3))  // one row above
    assert(!Viewport.contains(2, 9))  // one row below
  }

  test("scrollDelta returns -ticksPerDetent for an Up scroll inside the viewport") {
    val ev = MouseEvent.Scroll(ScrollDirection.Up, col = 5, row = 6, mods = NoMods)
    assert(LogView.scrollDelta(ev, Viewport) == Some(-3))
  }

  test("scrollDelta returns +ticksPerDetent for a Down scroll inside the viewport") {
    val ev = MouseEvent.Scroll(ScrollDirection.Down, col = 5, row = 6, mods = NoMods)
    assert(LogView.scrollDelta(ev, Viewport) == Some(3))
  }

  test("scrollDelta honours a custom ticksPerDetent") {
    val ev = MouseEvent.Scroll(ScrollDirection.Down, col = 5, row = 6, mods = NoMods)
    assert(LogView.scrollDelta(ev, Viewport, ticksPerDetent = 1) == Some(1))
  }

  test("scrollDelta drops scrolls that land outside the viewport") {
    val outside = MouseEvent.Scroll(ScrollDirection.Up, col = 1, row = 4, mods = NoMods)
    assert(LogView.scrollDelta(outside, Viewport).isEmpty)
  }

  test("scrollDelta ignores horizontal scroll directions") {
    val left  = MouseEvent.Scroll(ScrollDirection.Left, col = 5, row = 6, mods = NoMods)
    val right = MouseEvent.Scroll(ScrollDirection.Right, col = 5, row = 6, mods = NoMods)
    assert(LogView.scrollDelta(left, Viewport).isEmpty)
    assert(LogView.scrollDelta(right, Viewport).isEmpty)
  }

  test("scrollDelta ignores non-Scroll mouse events") {
    val click = MouseEvent.Press(MouseButton.Left, col = 5, row = 6, mods = NoMods)
    assert(LogView.scrollDelta(click, Viewport).isEmpty)
  }
