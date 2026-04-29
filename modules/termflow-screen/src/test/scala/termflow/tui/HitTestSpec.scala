package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.Layout.asLayout
import termflow.tui.ScreenPrelude.*

class HitTestSpec extends AnyFunSuite:

  // --- Rect ---------------------------------------------------------------

  test("Rect.contains is inclusive on both edges"):
    val r = Rect(x = 5, y = 3, width = 4, height = 2)
    assert(r.contains(5, 3))
    assert(r.contains(8, 4)) // 5 + 4 - 1 = 8 ; 3 + 2 - 1 = 4
    assert(!r.contains(4, 3))
    assert(!r.contains(9, 4))
    assert(!r.contains(5, 5))

  test("Rect.contains is false for empty rectangles"):
    assert(!Rect(x = 1, y = 1, width = 0, height = 5).contains(1, 1))
    assert(!Rect(x = 1, y = 1, width = 5, height = 0).contains(1, 1))
    assert(!Rect.empty.contains(1, 1))

  test("Rect.at builds from a Coord origin"):
    val r = Rect.at(Coord(3.x, 4.y), width = 5, height = 6)
    assert(r.x == 3 && r.y == 4 && r.width == 5 && r.height == 6)

  // --- HitTest ------------------------------------------------------------

  test("hit returns the topmost zone under the click"):
    val reg = HitTest
      .empty[String]
      .add("background", Rect(1, 1, 10, 10))
      .add("button", Rect(3, 3, 4, 2))
    assert(reg.hit(4, 4).contains("button"), "button overlays the background")
    assert(reg.hit(1, 1).contains("background"), "outside the button — falls through")

  test("hit returns None when no zone covers the cell"):
    val reg = HitTest.empty[String].add("a", Rect(1, 1, 5, 5))
    assert(reg.hit(10, 10).isEmpty)

  test("HitTest.++ concatenates registries in order"):
    val a      = HitTest.of("a" -> Rect(1, 1, 5, 5))
    val b      = HitTest.of("b" -> Rect(2, 2, 3, 3))
    val merged = a ++ b
    assert(merged.size == 2)
    // b is later — should win on overlap.
    assert(merged.hit(3, 3).contains("b"))

  // --- Layout.Zone --------------------------------------------------------

  private def tn(text: String): TextNode =
    TextNode(1.x, 1.y, List(Text(text, Style())))

  test("resolveTracked records a Zone's resolved rect"):
    val layout       = Layout.zone("title", tn("Hello"))
    val (nodes, reg) = Layout.resolveTracked[String](layout, at = Coord(2.x, 3.y))
    assert(nodes.size == 1, "the zone wrapper is transparent to node output")
    assert(reg.size == 1)
    val (id, rect) = reg.zones.head
    assert(id == "title")
    // Natural size of "Hello" is (5, 1); origin is (2, 3).
    assert(rect.x == 2 && rect.y == 3)
    assert(rect.width == 5 && rect.height == 1)

  test("resolveTracked threads ids through Row/Column children"):
    val layout = Layout.Column(
      gap = 0,
      children = List(
        Layout.zone("row-1", tn("A")),
        Layout.zone("row-2", tn("BC")),
        Layout.zone("row-3", tn("DEF"))
      )
    )
    val (_, reg) = Layout.resolveTracked[String](layout)
    assert(reg.size == 3)
    val rows = reg.zones.toList
    assert(rows.map(_._1) == List("row-1", "row-2", "row-3"))
    // Successive y values, x stays at 1.
    assert(rows.head._2.y == 1)
    assert(rows(1)._2.y == 2)
    assert(rows(2)._2.y == 3)

  test("resolveTracked records the post-Fill rectangle when there's a budget"):
    val box = BoxNode(1.x, 1.y, 5, 1, children = Nil, Style(border = true))
    val layout = Layout.Row(
      gap = 0,
      children = List(
        Layout.Fill(Layout.zone("fill", Layout.Elem(box))),
        Layout.Elem(tn("end"))
      )
    )
    val (_, reg) = Layout.resolveTracked[String](layout, at = Coord(1.x, 1.y), availableWidth = 20, availableHeight = 1)
    assert(reg.size == 1)
    val (id, rect) = reg.zones.head
    assert(id == "fill")
    // tn("end") natural width is 3, so fill consumes 17 cells.
    assert(rect.width == 17, s"fill rect should be 17 cells wide, got ${rect.width}")

  test("resolveTracked records all zones — caller picks an id type"):
    // Erasure makes runtime type filtering impossible, so apps must use a
    // uniform id type per tracked tree. Mixed-type trees still resolve;
    // the registry just contains whatever the call sites supplied.
    val layout = Layout.Column(
      gap = 0,
      children = List(
        Layout.zone("a", tn("first")),
        Layout.zone("b", tn("second"))
      )
    )
    val (_, reg) = Layout.resolveTracked[String](layout)
    assert(reg.size == 2)
    assert(reg.zones.map(_._1).toList == List("a", "b"))

  test("resolveTracked id type can be a sealed enum"):
    enum ZoneId:
      case Header, Body, Footer
    val layout = Layout.Column(
      gap = 0,
      children = List(
        Layout.zone(ZoneId.Header, tn("h")),
        Layout.zone(ZoneId.Body, tn("b")),
        Layout.zone(ZoneId.Footer, tn("f"))
      )
    )
    val (_, reg) = Layout.resolveTracked[ZoneId](layout)
    assert(reg.size == 3)
    assert(reg.hit(1, 1).contains(ZoneId.Header))
    assert(reg.hit(1, 2).contains(ZoneId.Body))
    assert(reg.hit(1, 3).contains(ZoneId.Footer))
