package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.KeyDecoder.InputKey

class ListViewSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private val items3 = Vector("alpha", "beta", "gamma")
  private val items5 = Vector("a", "b", "c", "d", "e")

  private def submit: String => Option[String] = _ => None

  // --- state construction --------------------------------------------------

  test("State.empty starts with no items, selection 0, scroll 0"):
    val s = ListView.State.empty[String](visibleRows = 4)
    assert(s.isEmpty)
    assert(s.size == 0)
    assert(s.selected == 0)
    assert(s.scrollOffset == 0)
    assert(s.visibleRows == 4)
    assert(s.selectedItem.isEmpty)

  test("State.of pre-populates and parks selection at index 0"):
    val s = ListView.State.of(items3, visibleRows = 5)
    assert(s.size == 3)
    assert(s.selectedItem.contains("alpha"))

  test("State.empty clamps visibleRows to >= 1"):
    val s = ListView.State.empty[String](visibleRows = 0)
    assert(s.visibleRows == 1)

  test("withItems clamps selected and scrollOffset when the list shrinks"):
    val s0 = ListView.State.of(items5, visibleRows = 3).copy(selected = 4, scrollOffset = 2)
    val s1 = s0.withItems(Vector("x", "y"))
    assert(s1.size == 2)
    assert(s1.selected == 1)
    assert(s1.scrollOffset == 0)

  test("withItems of an empty collection resets everything to zero"):
    val s = ListView.State.of(items5).copy(selected = 3)
    val e = s.withItems(Vector.empty)
    assert(e.isEmpty)
    assert(e.selected == 0)
    assert(e.scrollOffset == 0)

  // --- handleKey: empty list -----------------------------------------------

  test("handleKey on an empty list is a no-op for every key"):
    val s0      = ListView.State.empty[String](visibleRows = 4)
    val (s1, m) = ListView.handleKey(s0, InputKey.ArrowDown)(submit)
    assert(s1 == s0 && m.isEmpty)

  // --- handleKey: movement -------------------------------------------------

  test("ArrowDown advances selection, clamping at the last item"):
    val s0      = ListView.State.of(items3)
    val (s1, _) = ListView.handleKey(s0, InputKey.ArrowDown)(submit)
    assert(s1.selected == 1)
    val (s2, _) = ListView.handleKey(s1, InputKey.ArrowDown)(submit)
    assert(s2.selected == 2)
    val (s3, _) = ListView.handleKey(s2, InputKey.ArrowDown)(submit)
    assert(s3.selected == 2, "clamped at end, no overflow")

  test("ArrowUp retreats selection, clamping at 0"):
    val s0      = ListView.State.of(items3).copy(selected = 2)
    val (s1, _) = ListView.handleKey(s0, InputKey.ArrowUp)(submit)
    assert(s1.selected == 1)
    val (s2, _) = ListView.handleKey(s1, InputKey.ArrowUp)(submit)
    val (s3, _) = ListView.handleKey(s2, InputKey.ArrowUp)(submit)
    assert(s3.selected == 0, "clamped at 0, no underflow")

  test("Home and End jump to the endpoints"):
    val s0     = ListView.State.of(items5).copy(selected = 2)
    val (h, _) = ListView.handleKey(s0, InputKey.Home)(submit)
    assert(h.selected == 0)
    val (e, _) = ListView.handleKey(s0, InputKey.End)(submit)
    assert(e.selected == 4)

  // --- handleKey: Enter / Space --------------------------------------------

  test("Enter on the selected item calls onSelect and dispatches its message"):
    val s0      = ListView.State.of(items3).copy(selected = 1)
    val (s1, m) = ListView.handleKey(s0, InputKey.Enter)(item => Some(s"chose:$item"))
    assert(s1 == s0, "Enter must not mutate state")
    assert(m.contains("chose:beta"))

  test("Space also activates onSelect"):
    val s0     = ListView.State.of(items3).copy(selected = 2)
    val (_, m) = ListView.handleKey(s0, InputKey.CharKey(' '))(item => Some(item))
    assert(m.contains("gamma"))

  test("Enter when onSelect returns None dispatches nothing"):
    val s0     = ListView.State.of(items3)
    val (_, m) = ListView.handleKey(s0, InputKey.Enter)(_ => None)
    assert(m.isEmpty)

  // --- scroll viewport -----------------------------------------------------

  test("scrolling down past the viewport advances scrollOffset"):
    val s0 = ListView.State.of(items5, visibleRows = 3)
    // Walk selection to 3 (past the 3-row window).
    val s1 = (1 to 3).foldLeft(s0) { case (acc, _) =>
      ListView.handleKey(acc, InputKey.ArrowDown)(submit)._1
    }
    assert(s1.selected == 3)
    assert(s1.scrollOffset == 1, "viewport should have scrolled down by 1")

  test("scrolling up above the viewport decreases scrollOffset"):
    val s0 = ListView.State.of(items5, visibleRows = 2).copy(selected = 3, scrollOffset = 2)
    // Selected=3 is visible at offset 2 (rows 2,3). Move up one more.
    val (s1, _) = ListView.handleKey(s0, InputKey.ArrowUp)(submit)
    assert(s1.selected == 2)
    assert(s1.scrollOffset == 2, "still visible; offset unchanged")
    val (s2, _) = ListView.handleKey(s1, InputKey.ArrowUp)(submit)
    assert(s2.selected == 1)
    assert(s2.scrollOffset == 1, "scrolled up one row")

  test("End places the last item in the bottom-most viewport row"):
    val s0      = ListView.State.of(items5, visibleRows = 2)
    val (s1, _) = ListView.handleKey(s0, InputKey.End)(submit)
    assert(s1.selected == 4)
    assert(s1.scrollOffset == 3, "last item shown at row 2 of a 2-row window")

  // --- rendering -----------------------------------------------------------

  private def render(v: VNode, width: Int, height: Int): (Array[Array[Char]], Array[Array[Style]]) =
    val frame = AnsiRenderer.buildFrame(RootNode(width, height, List(v), None))
    val chars = Array.tabulate(height, width)((r, c) => frame.cells(r)(c).ch)
    val style = Array.tabulate(height, width)((r, c) => frame.cells(r)(c).style)
    (chars, style)

  test("unfocused render shows every item with an unstyled prefix"):
    val s       = ListView.State.of(items3, visibleRows = 3)
    val v       = ListView.view(s, lineWidth = 10, focused = false, render = (s: String) => s)
    val (cs, _) = render(v, 10, 3)
    val rows    = cs.map(_.mkString)
    assert(rows(0).startsWith("  alpha"))
    assert(rows(1).startsWith("  beta"))
    assert(rows(2).startsWith("  gamma"))

  test("focused render highlights the selected row in inverse video"):
    val s            = ListView.State.of(items3, visibleRows = 3).copy(selected = 1)
    val v            = ListView.view(s, lineWidth = 10, focused = true, render = (s: String) => s)
    val (cs, styles) = render(v, 10, 3)
    val row1         = cs(1).mkString
    assert(row1.startsWith("▸ beta"))
    // All cells of row 1 use theme.primary as background.
    (0 until 10).foreach(c => assert(styles(1)(c).bg == Theme.dark.primary, s"row 1 col $c expected inverse bg"))
    // Rows 0 and 2 do not.
    assert(styles(0)(0).bg == Color.Default)
    assert(styles(2)(0).bg == Color.Default)

  test("unfocused render does NOT mark the selection (focus gates the cursor)"):
    val s            = ListView.State.of(items3, visibleRows = 3).copy(selected = 1)
    val v            = ListView.view(s, lineWidth = 10, focused = false, render = (s: String) => s)
    val (cs, styles) = render(v, 10, 3)
    // No inverse video on the selected row…
    assert(styles(1)(0).bg == Color.Default)
    // …and no ▸ marker either; every row has the "  " prefix so column
    // alignment stays stable across focus transitions.
    assert(cs(1).mkString.startsWith("  beta"))
    assert(!cs.exists(_.contains('▸')))

  test("viewport scrolls so the selected item is visible"):
    val s0      = ListView.State.of(items5, visibleRows = 2).copy(selected = 4, scrollOffset = 3)
    val v       = ListView.view(s0, lineWidth = 6, focused = true, render = (s: String) => s)
    val (cs, _) = render(v, 6, 2)
    // With selected=4, scrollOffset=3, vis=2: rows should show items[3]='d' and items[4]='e'.
    assert(cs(0).mkString.trim.endsWith("d"))
    assert(cs(1).mkString.trim.endsWith("e"))

  test("empty items render as blank filler rows sized to visibleRows"):
    val s       = ListView.State.empty[String](visibleRows = 3)
    val v       = ListView.view(s, lineWidth = 5, focused = true)
    val (cs, _) = render(v, 5, 3)
    cs.foreach(row => assert(row.mkString == "     "))

  test("a viewport taller than the list fills remaining rows with blanks"):
    val s       = ListView.State.of(Vector("one"), visibleRows = 3)
    val v       = ListView.view(s, lineWidth = 8, focused = false, render = (s: String) => s)
    val (cs, _) = render(v, 8, 3)
    assert(cs(0).mkString.startsWith("  one"))
    // Rows 1 and 2 should be blank — no ghost items.
    assert(cs(1).mkString == "        ")
    assert(cs(2).mkString == "        ")

  test("renders overflow items truncated to (lineWidth - 2)"):
    val long    = Vector("this-is-a-very-long-item-name")
    val s       = ListView.State.of(long, visibleRows = 1)
    val v       = ListView.view(s, lineWidth = 8, focused = false, render = (s: String) => s)
    val (cs, _) = render(v, 8, 1)
    // "  " prefix (unfocused, no cursor) + 6 chars of truncated content.
    assert(cs(0).mkString == "  this-i")

  test("width helper reports >= 3 regardless of input"):
    assert(ListView.width(30) == 30)
    assert(ListView.width(2) == 3)
    assert(ListView.width(-1) == 3)

  test("ListView composes inside Layout.column with its full visibleRows height"):
    val s = ListView.State.of(items5, visibleRows = 4)
    val v = ListView.view(s, lineWidth = 12)
    val c = Layout.column(gap = 0)(v)
    assert(c.measure == (12, 4))
