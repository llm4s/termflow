package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.KeyDecoder.InputKey

class SelectSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private val roles                          = Vector("Admin", "Editor", "Viewer")
  private def noop: String => Option[String] = _ => None

  // --- State construction --------------------------------------------------

  test("State.empty starts closed with no items"):
    val s = Select.State.empty[String]()
    assert(s.isEmpty)
    assert(!s.open)
    assert(s.value.isEmpty)

  test("State.of pre-populates and parks selection on the first item, closed"):
    val s = Select.State.of(roles)
    assert(s.size == 3)
    assert(s.value.contains("Admin"))
    assert(!s.open)

  test("opened / closed / toggled manage the visibility flag"):
    val s = Select.State.of(roles)
    assert(s.opened.open)
    assert(!s.opened.closed.open)
    assert(s.toggled.open)
    assert(!s.toggled.toggled.open)

  test("selectIndex clamps and updates the underlying ListView selection"):
    val s = Select.State.of(roles)
    assert(s.selectIndex(2).value.contains("Viewer"))
    assert(s.selectIndex(99).value.contains("Viewer"))
    assert(s.selectIndex(-5).value.contains("Admin"))

  test("selectIndex on an empty state is a no-op"):
    val s = Select.State.empty[String]()
    assert(s.selectIndex(0) == s)

  test("withItems replaces the item set and delegates clamping"):
    val s = Select.State.of(roles).selectIndex(2)
    val t = s.withItems(Vector("one", "two"))
    assert(t.size == 2)
    assert(t.value.contains("two")) // clamp from index 2 to index 1

  // --- handleKey: closed ---------------------------------------------------

  test("closed + Enter opens the dropdown"):
    val s0      = Select.State.of(roles)
    val (s1, m) = Select.handleKey(s0, InputKey.Enter)(noop)
    assert(s1.open)
    assert(m.isEmpty, "opening does not dispatch a change")

  test("closed + Space opens the dropdown"):
    val s0      = Select.State.of(roles)
    val (s1, _) = Select.handleKey(s0, InputKey.CharKey(' '))(noop)
    assert(s1.open)

  test("closed + ArrowDown opens the dropdown (desktop-combobox convention)"):
    val s0      = Select.State.of(roles)
    val (s1, _) = Select.handleKey(s0, InputKey.ArrowDown)(noop)
    assert(s1.open)

  test("closed + any other key is a no-op"):
    val s0      = Select.State.of(roles)
    val (s1, m) = Select.handleKey(s0, InputKey.CharKey('x'))(noop)
    assert(s1 == s0 && m.isEmpty)

  test("closed + Enter on an empty Select stays closed"):
    val s0      = Select.State.empty[String]()
    val (s1, m) = Select.handleKey(s0, InputKey.Enter)(noop)
    assert(!s1.open)
    assert(m.isEmpty)

  // --- handleKey: open -----------------------------------------------------

  test("open + ArrowDown advances selection via the underlying ListView"):
    val s0      = Select.State.of(roles).opened
    val (s1, _) = Select.handleKey(s0, InputKey.ArrowDown)(noop)
    assert(s1.open)
    assert(s1.value.contains("Editor"))

  test("open + ArrowUp retreats via the underlying ListView"):
    val s0      = Select.State.of(roles).selectIndex(2).opened
    val (s1, _) = Select.handleKey(s0, InputKey.ArrowUp)(noop)
    assert(s1.value.contains("Editor"))

  test("open + Home / End jump to the ends"):
    val s0     = Select.State.of(roles).selectIndex(1).opened
    val (h, _) = Select.handleKey(s0, InputKey.Home)(noop)
    assert(h.value.contains("Admin"))
    val (e, _) = Select.handleKey(s0, InputKey.End)(noop)
    assert(e.value.contains("Viewer"))

  test("open + Enter commits the current selection, closes, and dispatches onChange"):
    val s0      = Select.State.of(roles).selectIndex(1).opened
    val (s1, m) = Select.handleKey(s0, InputKey.Enter)(v => Some(s"chose:$v"))
    assert(!s1.open, "Enter commits and closes")
    assert(s1.value.contains("Editor"))
    assert(m.contains("chose:Editor"))

  test("open + Space also commits and closes"):
    val s0      = Select.State.of(roles).selectIndex(2).opened
    val (s1, m) = Select.handleKey(s0, InputKey.CharKey(' '))(v => Some(v))
    assert(!s1.open)
    assert(m.contains("Viewer"))

  test("open + Escape closes without changing selection or dispatching"):
    val s0      = Select.State.of(roles).selectIndex(1).opened
    val (s1, m) = Select.handleKey(s0, InputKey.Escape)(noop)
    assert(!s1.open)
    assert(s1.value.contains("Editor"), "selection preserved")
    assert(m.isEmpty)

  test("open + other key leaves state unchanged"):
    val s0      = Select.State.of(roles).opened
    val (s1, m) = Select.handleKey(s0, InputKey.CharKey('x'))(noop)
    assert(s1 == s0 && m.isEmpty)

  // --- rendering -----------------------------------------------------------

  private def render(v: VNode, width: Int, height: Int): (Array[Array[Char]], Array[Array[Style]]) =
    val frame = AnsiRenderer.buildFrame(RootNode(width, height, List(v), None))
    val chars = Array.tabulate(height, width)((r, c) => frame.cells(r)(c).ch)
    val style = Array.tabulate(height, width)((r, c) => frame.cells(r)(c).style)
    (chars, style)

  test("closed render shows ▾ + selected value in one row"):
    val s       = Select.State.of(roles)
    val v       = Select.view(s, lineWidth = 12, focused = false)
    val (cs, _) = render(v, 12, 1)
    assert(cs(0).mkString.startsWith("▾ Admin"))

  test("closed + focused render paints the header in inverse video"):
    val s           = Select.State.of(roles)
    val v           = Select.view(s, lineWidth = 10, focused = true)
    val (_, styles) = render(v, 10, 1)
    (0 until 10).foreach(c => assert(styles(0)(c).bg == Theme.dark.primary, s"header col $c expected inverse bg"))

  test("closed render on empty Select shows (none)"):
    val s       = Select.State.empty[String]()
    val v       = Select.view(s, lineWidth = 10, focused = false)
    val (cs, _) = render(v, 10, 1)
    assert(cs(0).mkString.contains("(none)"))

  test("open render shows ▴ header plus the visible-row viewport beneath"):
    val s       = Select.State.of(roles, visibleRows = 3).opened
    val v       = Select.view(s, lineWidth = 12, focused = true)
    val (cs, _) = render(v, 12, Select.maxHeight(3))
    // Row 0: header with ▴ indicator.
    assert(cs(0).mkString.startsWith("▴ Admin"))
    // Rows 1..3: the three options (Admin is selected/focused → row 1 starts ▸).
    assert(cs(1).mkString.startsWith("▸ Admin"))
    assert(cs(2).mkString.startsWith("  Editor"))
    assert(cs(3).mkString.startsWith("  Viewer"))

  test("open render delegates cursor highlighting to ListView"):
    val s           = Select.State.of(roles, visibleRows = 3).selectIndex(2).opened
    val v           = Select.view(s, lineWidth = 10, focused = true)
    val (_, styles) = render(v, 10, Select.maxHeight(3))
    // Row 3 ("Viewer") is the selected option → inverse video.
    (0 until 10).foreach(c => assert(styles(3)(c).bg == Theme.dark.primary, s"row 3 col $c expected inverse bg"))
    // Row 1 ("Admin") is not selected → default bg.
    assert(styles(1)(0).bg == Color.Default)

  test("Select.maxHeight reflects 1-header + visibleRows"):
    assert(Select.maxHeight(5) == 6)
    assert(Select.maxHeight(0) == 2) // visibleRows clamped to 1

  test("width helper reports >= 3 regardless of input"):
    assert(Select.width(20) == 20)
    assert(Select.width(1) == 3)
    assert(Select.width(-5) == 3)

  test("Select.view composes inside Layout.column at both heights"):
    val closed    = Select.State.of(roles, visibleRows = 3)
    val open      = closed.opened
    val closedLay = Layout.column(gap = 0)(Select.view(closed, lineWidth = 10))
    val openLay   = Layout.column(gap = 0)(Select.view(open, lineWidth = 10))
    assert(closedLay.measure == (10, 1))
    assert(openLay.measure == (10, Select.maxHeight(3)))
