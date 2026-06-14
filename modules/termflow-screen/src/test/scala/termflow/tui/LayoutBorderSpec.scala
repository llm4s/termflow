package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.ScreenPrelude.*

class LayoutBorderSpec extends AnyFunSuite:

  private def tn(text: String): TextNode =
    TextNode(1.x, 1.y, List(Text(text, Style())))

  // ---- measure ------------------------------------------------------------

  test("Border with all None measures to (0, 0)"):
    val b = Layout.border()
    assert(b.measure == (0, 0))

  test("Border with only center measures to center's natural size"):
    val b = Layout.border(center = Layout.Elem(tn("hello")))
    assert(b.measure == (5, 1))

  test("Border with top + center measures with vertical gap when present"):
    val b = Layout.border(
      top = Layout.Elem(tn("header")),
      center = Layout.Elem(tn("body")),
      gap = 1
    )
    val (w, h) = b.measure
    assert(w == 6)         // max(top, center) widths
    assert(h == 1 + 1 + 1) // top + gap + center

  // ---- resolveTo ----------------------------------------------------------

  test("Border lays out top across full width and center underneath"):
    // 30 × 10 budget. top = 1-row label, no gap, center = 1-row body.
    // Top occupies row 1, center band has height 9 starting at row 2.
    val b = Layout.border(
      top = Layout.Elem(tn("hello")),
      center = Layout.Elem(tn("body"))
    )
    val nodes = Layout.resolveTo(b, Coord(1.x, 1.y), 30, 10)
    val texts = nodes.collect { case t: TextNode => (t.x.value, t.y.value) }.toSet
    assert(texts.contains((1, 1))) // top at y = 1
    assert(texts.contains((1, 2))) // center begins at y = 2

  test("Border with all five zones positions left/right at the band edges"):
    val b = Layout.border(
      top = Layout.Elem(tn("T")),
      left = Layout.Elem(tn("L")),
      center = Layout.Elem(tn("C")),
      right = Layout.Elem(tn("R")),
      bottom = Layout.Elem(tn("B")),
      gap = 0
    )
    // 20 × 5 budget. top = 1 row, bottom = 1 row, middle = 3 rows.
    // left.w = 1, right.w = 1, center.w = 18.
    val (_, hits) = Layout.resolveTracked[String](
      Layout.border(
        top = Layout.zone("top", tn("T")),
        left = Layout.zone("left", tn("L")),
        center = Layout.zone("center", tn("C")),
        right = Layout.zone("right", tn("R")),
        bottom = Layout.zone("bottom", tn("B")),
        gap = 0
      ),
      Coord(1.x, 1.y),
      20,
      5
    )
    // Top spans full width on row 1.
    assert(hits.hit(10, 1).contains("top"))
    // Bottom spans full width on row 5.
    assert(hits.hit(10, 5).contains("bottom"))
    // Middle band: rows 2-4. Left at x=1, right at x=20.
    assert(hits.hit(1, 3).contains("left"))
    assert(hits.hit(20, 3).contains("right"))
    // Center fills x=2..19 on rows 2..4.
    assert(hits.hit(10, 3).contains("center"))

  test("Border insets top/bottom so corner columns stay clear of left/right (issue #209)"):
    val (_, hits) = Layout.resolveTracked[String](
      Layout.border(
        top = Layout.zone("top", tn("T")),
        left = Layout.zone("left", tn("L")),
        center = Layout.zone("center", tn("C")),
        right = Layout.zone("right", tn("R")),
        bottom = Layout.zone("bottom", tn("B")),
        gap = 0
      ),
      Coord(1.x, 1.y),
      20,
      5
    )
    // left.w = right.w = 1, so top/bottom span only the middle columns
    // (x = 2..19). The corner columns (x = 1, x = 20) are left for the
    // left/right zones in the middle band and must not be claimed by
    // top/bottom — otherwise the corners overlap.
    assert(hits.hit(2, 1).contains("top"))
    assert(hits.hit(19, 1).contains("top"))
    assert(!hits.hit(1, 1).contains("top"))
    assert(!hits.hit(20, 1).contains("top"))
    assert(hits.hit(2, 5).contains("bottom"))
    assert(hits.hit(19, 5).contains("bottom"))
    assert(!hits.hit(1, 5).contains("bottom"))
    assert(!hits.hit(20, 5).contains("bottom"))

  test("Border collapses the middle band when none of left/center/right are set"):
    val b = Layout.border(
      top = Layout.Elem(tn("T")),
      bottom = Layout.Elem(tn("B")),
      gap = 0
    )
    val nodes = Layout.resolveTo(b, Coord(1.x, 1.y), 10, 5)
    val ys    = nodes.collect { case t: TextNode => t.y.value }.toSet
    // top at 1, bottom at 5; nothing in the middle.
    assert(ys.contains(1))
    assert(ys.contains(5))
    assert(!ys.contains(3))

  test("Border resolves at natural size when no width/height budget is given"):
    // Calling resolveTo with availableWidth/availableHeight = -1 falls back to
    // the natural-size path inside resolveBorder (and exercises measureBorder
    // for the band totals).
    val b = Layout.border(
      top = Layout.Elem(tn("TT")),      // (2, 1)
      left = Layout.Elem(tn("LL")),     // (2, 1)
      center = Layout.Elem(tn("CCCC")), // (4, 1)
      right = Layout.Elem(tn("RR")),    // (2, 1)
      bottom = Layout.Elem(tn("BBBB")), // (4, 1)
      gap = 1
    )
    val nodes = Layout.resolveTo(b, Coord(1.x, 1.y), availableWidth = -1, availableHeight = -1)
    val ys    = nodes.collect { case t: TextNode => t.y.value }.toSet
    // top at row 1; gap row 2; mid row 3; gap row 4; bottom row 5.
    assert(ys.contains(1))
    assert(ys.contains(3))
    assert(ys.contains(5))

  test("measureBorder returns (0, 0) for an entirely empty border"):
    assert(Layout.border().measure == (0, 0))

  test("Layout.resolve uses default origin (1, 1)"):
    // Hits the resolve$default$1 synthetic accessor on the trait method.
    val nodes  = Layout.Elem(tn("hi")).resolve()
    val coords = nodes.collect { case t: TextNode => (t.x.value, t.y.value) }
    assert(coords == List((1, 1)))

  test("Border respects the gap between top, middle, and bottom bands"):
    val b = Layout.border(
      top = Layout.Elem(tn("T")),
      center = Layout.Elem(tn("C")),
      bottom = Layout.Elem(tn("B")),
      gap = 2
    )
    // 20 × 10. Top = 1 row at y=1. Then 2-row gap (y=2,3). Mid at y=4..6 (3 rows).
    // Then 2-row gap (y=7,8). Bottom at y=9.
    // Wait: midH = 10 - 1 (top) - 1 (bottom) - 2 (gap-above) - 2 (gap-below) = 4.
    // So mid occupies y=4..7 inclusive. Bottom at y = 4 + 4 + 2 = 10.
    val nodes = Layout.resolveTo(b, Coord(1.x, 1.y), 20, 10)
    val ys    = nodes.collect { case t: TextNode => t.y.value }.toSet
    assert(ys.contains(1))  // top
    assert(ys.contains(4))  // center (mid band starts here)
    assert(ys.contains(10)) // bottom
