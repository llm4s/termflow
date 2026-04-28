package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class TabsSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private def textOf(node: VNode): String = node match
    case TextNode(_, _, runs) => runs.map(_.txt).mkString
    case _                    => fail(s"expected TextNode, got $node")

  test("renders one cell per tab plus separators") {
    val node = Tabs(Seq("Home", "Work", "Notes"), activeIndex = 0)
    val s    = textOf(node)
    assert(s.contains("Home"))
    assert(s.contains("Work"))
    assert(s.contains("Notes"))
  }

  test("active tab is wrapped in [ ] and unfocused tabs use spaces") {
    val s = textOf(Tabs(Seq("A", "B"), activeIndex = 1))
    assert(s.contains("[ B ]"), s"active tab should be bracketed: $s")
    assert(s.contains("  A  "), s"inactive tab should be padded: $s")
  }

  test("focused-but-inactive tab uses primary colour and bold") {
    val node = Tabs(Seq("A", "B"), activeIndex = 0, focusedIndex = 1)
    node match
      case TextNode(_, _, runs) =>
        // Run 0 = active "[ A ]", run 1 = separator, run 2 = focused " B "
        val focusedRun = runs(2)
        assert(focusedRun.style.bold, "focused tab label should be bold")
      case _ => fail("expected TextNode")
  }

  test("custom separator appears between tabs but not after the last") {
    val s   = textOf(Tabs(Seq("X", "Y", "Z"), activeIndex = 0, separator = " | "))
    val occ = s.split(" \\| ").length - 1
    assert(occ == 2, s"expected 2 separators between 3 tabs, got $occ in: $s")
  }

  test("empty separator produces no inter-tab gap") {
    val s = textOf(Tabs(Seq("AB", "CD"), activeIndex = 0, separator = ""))
    // active "[ AB ]" + inactive "  CD  " concatenated with no space.
    assert(s == "[ AB ]  CD  ", s"got '$s'")
  }

  test("Tabs.width sums tab cells + separators") {
    assert(Tabs.width(Seq("Home", "Work")) == ("Home".length + 4) + ("Work".length + 4) + 1)
    assert(Tabs.width(Seq("a"), separator = "") == 5) // "[ a ]" or "  a  "
    assert(Tabs.width(Seq.empty[String]) == 0)
  }

  test("hitTest maps a column offset to its tab index") {
    val labels = Seq("Home", "Work", "Notes")
    // Tab cell widths: 8, 8, 9. Separator length 1 (default " ").
    // [Home: 0..7] [sep: 8] [Work: 9..16] [sep: 17] [Notes: 18..26]
    assert(Tabs.hitTest(labels, colOffset = 0).contains(0))
    assert(Tabs.hitTest(labels, colOffset = 7).contains(0))
    assert(Tabs.hitTest(labels, colOffset = 8).isEmpty, "click on the separator should miss")
    assert(Tabs.hitTest(labels, colOffset = 9).contains(1))
    assert(Tabs.hitTest(labels, colOffset = 16).contains(1))
    assert(Tabs.hitTest(labels, colOffset = 18).contains(2))
    assert(Tabs.hitTest(labels, colOffset = 26).contains(2))
    assert(Tabs.hitTest(labels, colOffset = 27).isEmpty, "click past the last tab should miss")
    assert(Tabs.hitTest(labels, colOffset = -1).isEmpty)
  }

  test("Tabs is positioned at the supplied coordinate") {
    val node = Tabs(Seq("X"), activeIndex = 0, at = Coord(10.x, 4.y))
    node match
      case TextNode(x, y, _) =>
        assert(x.value == 10)
        assert(y.value == 4)
      case _ => fail("expected TextNode")
  }
