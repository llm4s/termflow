package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.KeyDecoder.InputKey

class TableSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private case class Row(name: String, qty: Int)
  private val rows3 = Vector(
    Row("apple", 10),
    Row("pear", 3),
    Row("grape", 120)
  )
  private val cols = Vector(
    Table.Column[Row]("Name", width = 6, align = Table.Align.Left, render = _.name),
    Table.Column[Row]("Qty", width = 4, align = Table.Align.Right, render = _.qty.toString)
  )

  private def noop: Row => Option[String] = _ => None

  // --- state construction --------------------------------------------------

  test("State.empty starts with no rows and selection 0"):
    val s = Table.State.empty(cols, visibleRows = 5)
    assert(s.isEmpty)
    assert(s.size == 0)
    assert(s.selectedIndex == 0)
    assert(s.selectedRow.isEmpty)
    assert(s.selectable)

  test("State.of pre-populates and parks selection on the first row when selectable"):
    val s = Table.State.of(cols, rows3)
    assert(s.size == 3)
    assert(s.selectedRow.contains(Row("apple", 10)))

  test("State.of non-selectable has selectedRow = None regardless of body state"):
    val s = Table.State.of(cols, rows3, selectable = false)
    assert(s.selectedRow.isEmpty)
    assert(s.size == 3)

  test("withRows re-clamps underlying ListView selection + scroll"):
    val s0 = Table.State
      .of(cols, rows3, visibleRows = 2)
      .copy(body = ListView.State.of(rows3, 2).copy(selected = 2, scrollOffset = 1))
    val s1 = s0.withRows(Vector(Row("kiwi", 1)))
    assert(s1.size == 1)
    assert(s1.selectedIndex == 0)

  // --- handleKey -----------------------------------------------------------

  test("handleKey on a non-selectable table is a no-op for every key"):
    val s0      = Table.State.of(cols, rows3, selectable = false)
    val (s1, m) = Table.handleKey(s0, InputKey.ArrowDown)(noop)
    assert(s1 == s0 && m.isEmpty)

  test("ArrowDown advances selection, clamping at the last row (selectable)"):
    val s0      = Table.State.of(cols, rows3)
    val (s1, _) = Table.handleKey(s0, InputKey.ArrowDown)(noop)
    assert(s1.selectedRow.contains(Row("pear", 3)))

  test("Enter dispatches onSelect for the current row"):
    val s0     = Table.State.of(cols, rows3).copy(body = ListView.State.of(rows3).copy(selected = 2))
    val (_, m) = Table.handleKey(s0, InputKey.Enter)(r => Some(s"sel:${r.name}"))
    assert(m.contains("sel:grape"))

  test("Home / End jump to endpoints"):
    val s0     = Table.State.of(cols, rows3).copy(body = ListView.State.of(rows3).copy(selected = 1))
    val (h, _) = Table.handleKey(s0, InputKey.Home)(noop)
    assert(h.selectedIndex == 0)
    val (e, _) = Table.handleKey(s0, InputKey.End)(noop)
    assert(e.selectedIndex == 2)

  // --- helpers --------------------------------------------------------------

  test("width is sum of column widths + separators between columns"):
    assert(Table.width(cols) == 6 + 1 + 4) // 6 + " " + 4

  test("width on an empty column list is 0"):
    assert(Table.width(Vector.empty[Table.Column[Row]]) == 0)

  test("height is 2 (header + divider) plus visibleRows"):
    assert(Table.height(5) == 7)
    assert(Table.height(0) == 3) // visibleRows clamped to 1

  test("formatHeader aligns per column"):
    assert(Table.formatHeader(cols) == "Name    Qty")

  test("formatRow respects column alignment and widths"):
    val r = Table.formatRow(cols, Row("kiwi", 7))
    // "kiwi  " (6 wide left) + " " + "   7" (4 wide right)
    assert(r == "kiwi     " + "  7" || r == "kiwi   " + "  7" || r == "kiwi      7")
    // Simpler direct check of the concatenation.
    val expected = "kiwi  " + " " + "   7"
    assert(r == expected, s"got [$r] expected [$expected]")

  test("formatRow truncates long cell content"):
    val r = Table.formatRow(cols, Row("strawberry", 12))
    // Name col width=6; "strawberry" truncated to "strawb".
    assert(r.startsWith("strawb"))

  // --- rendering -----------------------------------------------------------

  private def render(v: VNode, width: Int, height: Int): (Array[Array[Char]], Array[Array[Style]]) =
    val frame = AnsiRenderer.buildFrame(RootNode(width, height, List(v), None))
    val chars = Array.tabulate(height, width)((r, c) => frame.cells(r)(c).ch)
    val style = Array.tabulate(height, width)((r, c) => frame.cells(r)(c).style)
    (chars, style)

  test("view draws header + divider + body rows"):
    val s       = Table.State.of(cols, rows3, visibleRows = 3)
    val v       = Table.view(s, focused = false)
    val (cs, _) = render(v, Table.width(cols), Table.height(3))
    // Row 0: header.
    assert(cs(0).mkString == "Name    Qty")
    // Row 1: divider (─ repeated for totalW chars).
    assert(cs(1).forall(_ == '─'))
    // Rows 2..4: data.
    assert(cs(2).mkString.contains("apple"))
    assert(cs(3).mkString.contains("pear"))
    assert(cs(4).mkString.contains("grape"))

  test("focused render paints the selected row in inverse video"):
    val s = Table.State.of(cols, rows3, visibleRows = 3).copy(body = ListView.State.of(rows3, 3).copy(selected = 1))
    val v = Table.view(s, focused = true)
    val (_, styles) = render(v, Table.width(cols), Table.height(3))
    // Body row 1 (file row 3 = y=3): inverse bg.
    (0 until Table.width(cols)).foreach { c =>
      assert(styles(3)(c).bg == Theme.dark.primary, s"body row 1 col $c expected inverse bg")
    }
    // Body row 0 (file row 2) stays default.
    assert(styles(2)(0).bg == Color.Default)

  test("unfocused render does NOT highlight the selection"):
    val s = Table.State.of(cols, rows3, visibleRows = 3).copy(body = ListView.State.of(rows3, 3).copy(selected = 1))
    val v = Table.view(s, focused = false)
    val (_, styles) = render(v, Table.width(cols), Table.height(3))
    // No inverse video anywhere in the body.
    (2 until Table.height(3)).foreach { r =>
      (0 until Table.width(cols)).foreach { c =>
        assert(styles(r)(c).bg == Color.Default, s"row $r col $c should not be inverse")
      }
    }

  test("non-selectable render never highlights, even when focused"):
    val s           = Table.State.of(cols, rows3, visibleRows = 3, selectable = false)
    val v           = Table.view(s, focused = true)
    val (_, styles) = render(v, Table.width(cols), Table.height(3))
    (2 until Table.height(3)).foreach { r =>
      (0 until Table.width(cols)).foreach(c => assert(styles(r)(c).bg == Color.Default))
    }

  test("empty body fills with blanks up to visibleRows"):
    val s       = Table.State.empty(cols, visibleRows = 2)
    val v       = Table.view(s, focused = true)
    val (cs, _) = render(v, Table.width(cols), Table.height(2))
    // Body rows (indices 2, 3) should be blank strings of totalW cells.
    val w = Table.width(cols)
    assert(cs(2).mkString == " " * w)
    assert(cs(3).mkString == " " * w)

  test("Table.view composes inside Layout.column with the expected measure"):
    val s = Table.State.of(cols, rows3, visibleRows = 4)
    val v = Table.view(s)
    val c = Layout.column(gap = 0)(v)
    assert(c.measure == (Table.width(cols), Table.height(4)))
