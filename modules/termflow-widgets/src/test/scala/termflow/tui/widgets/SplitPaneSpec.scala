package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class SplitPaneSpec extends AnyFunSuite:

  given Theme = Theme.dark

  // ---- layout (horizontal) -------------------------------------------------

  test("horizontal split at 0.5 splits a width=10 region into 5 + 5") {
    val l = SplitPane.layout(SplitPane.Direction.Horizontal, width = 10, height = 3)
    assert(l.first.width == 5)
    assert(l.second.width == 5)
    assert(l.first.height == 3)
    assert(l.second.height == 3)
    assert(l.first.at.x.value == 1)
    assert(l.second.at.x.value == 6)
    assert(l.divider.isEmpty)
  }

  test("horizontal split with gap reserves a divider rect") {
    val l = SplitPane.layout(SplitPane.Direction.Horizontal, width = 10, height = 3, splitRatio = 0.5, gap = 2)
    assert(l.divider.isDefined)
    val d = l.divider.get
    assert(d.width == 2)
    assert(d.at.x.value == l.first.at.x.value + l.first.width)
    assert(l.second.at.x.value == d.at.x.value + d.width)
  }

  test("split clamps splitRatio to [MinSizeRatio, 1 - MinSizeRatio]") {
    val a = SplitPane.layout(SplitPane.Direction.Horizontal, width = 100, height = 10, splitRatio = 0.0)
    val b = SplitPane.layout(SplitPane.Direction.Horizontal, width = 100, height = 10, splitRatio = 1.0)
    assert(a.first.width >= 5, "first pane must be at least MinSizeRatio of total")
    assert(b.second.width >= 5, "second pane must be at least MinSizeRatio of total")
  }

  test("vertical split at 0.5 splits a height=10 region into 5 + 5") {
    val l = SplitPane.layout(SplitPane.Direction.Vertical, width = 10, height = 10)
    assert(l.first.height == 5)
    assert(l.second.height == 5)
    assert(l.first.width == 10)
    assert(l.second.width == 10)
    assert(l.first.at.y.value == 1)
    assert(l.second.at.y.value == 6)
  }

  test("dividerRect returns the divider when gap > 0 and None otherwise") {
    val withGap = SplitPane.dividerRect(SplitPane.Direction.Horizontal, width = 20, height = 5, gap = 1)
    val without = SplitPane.dividerRect(SplitPane.Direction.Horizontal, width = 20, height = 5, gap = 0)
    assert(withGap.isDefined)
    assert(without.isEmpty)
  }

  // ---- apply (rendering) --------------------------------------------------

  test("apply dispatches each pane's render with the resolved geometry") {
    val seen   = scala.collection.mutable.ListBuffer.empty[(String, Int, Int)]
    val first  = (_: Coord, w: Int, h: Int) => { seen += (("first", w, h)); Nil }
    val second = (_: Coord, w: Int, h: Int) => { seen += (("second", w, h)); Nil }
    SplitPane(first, second, width = 12, height = 4)
    assert(seen.toList == List(("first", 6, 4), ("second", 6, 4)))
  }

  test("apply concatenates VNodes from both pane renderers") {
    val n1    = TextNode(1.x, 1.y, List(Text("L", Style())))
    val n2    = TextNode(1.x, 1.y, List(Text("R", Style())))
    val nodes = SplitPane((_, _, _) => List(n1), (_, _, _) => List(n2), width = 6, height = 1)
    assert(nodes == List(n1, n2))
  }
