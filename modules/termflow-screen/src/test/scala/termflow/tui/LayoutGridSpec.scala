package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.ScreenPrelude.*

class LayoutGridSpec extends AnyFunSuite:

  private def tn(text: String): TextNode =
    TextNode(1.x, 1.y, List(Text(text, Style())))

  // ---- placement / measure ------------------------------------------------

  test("empty grid measures to (0, 0)"):
    val g = Layout.Grid(columns = 3, rowGap = 0, colGap = 0, cells = Nil)
    assert(g.measure == (0, 0))

  test("grid wraps cells to the next row when columns are full"):
    // 5 cells, 2 columns → 3 rows (last row half-occupied).
    val g = Layout.grid(columns = 2, rowGap = 0, colGap = 0)(
      tn("aa"),
      tn("bb"),
      tn("cc"),
      tn("dd"),
      tn("ee")
    )
    val (w, h) = g.measure
    // Each column natural width = max of "aa"/"cc"/"ee" = 2; both cols equal.
    assert(w == 4)
    // 3 rows, height 1 each, no row gap.
    assert(h == 3)

  test("grid honours rowGap and colGap in measure"):
    val g = Layout.grid(columns = 2, rowGap = 1, colGap = 3)(
      tn("aa"),
      tn("bb"),
      tn("cc"),
      tn("dd")
    )
    val (w, h) = g.measure
    assert(w == 2 + 3 + 2) // col widths + colGap
    assert(h == 1 + 1 + 1) // 2 rows + rowGap

  test("grid colSpan reserves multiple columns; cursor skips past them"):
    // Layout intent:
    //   [ A spans 2 cols ]
    //   [ B ] [ C ]
    val g = Layout.Grid(
      columns = 2,
      rowGap = 0,
      colGap = 0,
      cells = List(
        GridCell(Layout.Elem(tn("AA")), colSpan = 2),
        GridCell(Layout.Elem(tn("B"))),
        GridCell(Layout.Elem(tn("C")))
      )
    )
    // 2 rows × 1 row each; col widths = max single-col widths = 1, 1.
    val (w, h) = g.measure
    assert(w == 2) // colSpan-only cells don't widen any column
    assert(h == 2)

  test("grid resolveTo splits the budget evenly across columns"):
    // 30-wide budget, 3 cols, no colGap → 10 cells per col.
    val g = Layout.grid(columns = 3, rowGap = 0, colGap = 0)(
      tn("a"),
      tn("b"),
      tn("c")
    )
    val nodes = Layout.resolveTo(g, Coord(1.x, 1.y), availableWidth = 30, availableHeight = 1)
    val xs    = nodes.collect { case t: TextNode => t.x.value }.sorted
    assert(xs == List(1, 11, 21))

  test("grid resolveTo splits height across rows"):
    // 2 cols, 4 cells → 2 rows. height budget 10 → 5 each.
    val g = Layout.grid(columns = 2, rowGap = 0, colGap = 0)(
      tn("a"),
      tn("b"),
      tn("c"),
      tn("d")
    )
    val nodes = Layout.resolveTo(g, Coord(1.x, 1.y), availableWidth = 10, availableHeight = 10)
    val ys    = nodes.collect { case t: TextNode => t.y.value }.distinct.sorted
    assert(ys == List(1, 6))

  test("grid colSpan cell's box covers the spanned columns inside resolveTo"):
    // 2 cols × 2 rows, 20-wide budget. Each col 10 wide.
    // First cell spans 2 cols → its assigned origin x = 1, width = 20.
    // We can't directly observe the assigned width via TextNode, but we can
    // observe it via a tracked Zone and a HitTest hit.
    val g = Layout.Grid(
      columns = 2,
      rowGap = 0,
      colGap = 0,
      cells = List(
        GridCell(Layout.zone("wide", tn("X")), colSpan = 2),
        GridCell(Layout.zone("b", tn("B"))),
        GridCell(Layout.zone("c", tn("C")))
      )
    )
    val (_, hits) = Layout.resolveTracked[String](g, Coord(1.x, 1.y), 20, 2)
    // Click somewhere inside the spanning cell's right half should hit "wide".
    assert(hits.hit(15, 1).contains("wide"))
    // Second-row cells partition the bottom row at x=1..10 and x=11..20.
    assert(hits.hit(5, 2).contains("b"))
    assert(hits.hit(15, 2).contains("c"))
