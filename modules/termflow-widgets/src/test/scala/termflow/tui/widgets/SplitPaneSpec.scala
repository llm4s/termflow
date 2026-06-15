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

  test("panes never overflow a small region (first + gap + second <= major)") {
    // Regression: independent max(1, ...) floors let the panes sum to more
    // than the region, so the second pane / divider spilled past the edge.
    for
      w     <- 1 to 6
      gap   <- 0 to 2
      ratio <- Seq(0.05, 0.5, 0.95)
    do
      val l = SplitPane.layout(SplitPane.Direction.Horizontal, width = w, height = 3, splitRatio = ratio, gap = gap)
      val dividerW = l.divider.map(_.width).getOrElse(0)
      assert(
        l.first.width + dividerW + l.second.width <= w,
        s"overflow at w=$w gap=$gap ratio=$ratio: ${l.first.width}+$dividerW+${l.second.width} > $w"
      )
      // The second pane must also stay within the region's right edge.
      assert(l.second.at.x.value + l.second.width - 1 <= w, s"second pane past edge at w=$w gap=$gap ratio=$ratio")
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

  // ---- handleMouse (drag-to-resize) ---------------------------------------

  private def mods = KeyDecoder.Modifiers(false, false, false)

  test("Press on the divider arms the drag") {
    val state = SplitPane.DragState.at(0.5)
    val l     = SplitPane.layout(SplitPane.Direction.Horizontal, width = 20, height = 5, splitRatio = 0.5, gap = 1)
    val d     = l.divider.get
    val ev    = MouseEvent.Press(MouseButton.Left, d.at.x.value, d.at.y.value, mods)
    val next  = SplitPane.handleMouse(state, ev, SplitPane.Direction.Horizontal, 20, 5, gap = 1)
    assert(next.dragging, "press on the divider should arm the drag")
  }

  test("Press outside the divider does nothing") {
    val state = SplitPane.DragState.at(0.5)
    val ev    = MouseEvent.Press(MouseButton.Left, 1, 1, mods)
    val next  = SplitPane.handleMouse(state, ev, SplitPane.Direction.Horizontal, 20, 5, gap = 1)
    assert(!next.dragging)
    assert(next.splitRatio == 0.5)
  }

  test("Drag while armed updates splitRatio based on column position") {
    val armed = SplitPane.DragState(0.5, dragging = true)
    val ev    = MouseEvent.Drag(MouseButton.Left, col = 15, row = 3, mods)
    val next  = SplitPane.handleMouse(armed, ev, SplitPane.Direction.Horizontal, 20, 5, gap = 1)
    // rel = (15 - 1) = 14 of 19 (= width - 1) → 14/19
    assert(math.abs(next.splitRatio - 14.0 / 19.0) < 1e-6, s"expected ~${14.0 / 19.0}, got ${next.splitRatio}")
    assert(next.dragging, "still dragging")
  }

  test("Release ends the drag and snaps to the cursor's ratio") {
    val armed = SplitPane.DragState(0.5, dragging = true)
    val ev    = MouseEvent.Release(MouseButton.Left, col = 5, row = 3, mods)
    val next  = SplitPane.handleMouse(armed, ev, SplitPane.Direction.Horizontal, 20, 5, gap = 1)
    assert(!next.dragging, "release ends the drag")
    // rel = (5 - 1) = 4 of 19 (= width - 1) → 4/19
    assert(math.abs(next.splitRatio - 4.0 / 19.0) < 1e-6, s"expected ${4.0 / 19.0}, got ${next.splitRatio}")
  }

  test("Release outside the region keeps the previous ratio (no snap-back)") {
    val armed = SplitPane.DragState(0.4, dragging = true)
    val ev    = MouseEvent.Release(MouseButton.Left, col = 200, row = 200, mods)
    val next  = SplitPane.handleMouse(armed, ev, SplitPane.Direction.Horizontal, 20, 5, gap = 1)
    assert(!next.dragging)
    assert(next.splitRatio == 0.4)
  }

  test("handleMouse does nothing when gap < 1 (no divider to pick)") {
    val state = SplitPane.DragState.at(0.5)
    val ev    = MouseEvent.Press(MouseButton.Left, 10, 3, mods)
    val next  = SplitPane.handleMouse(state, ev, SplitPane.Direction.Horizontal, 20, 5, gap = 0)
    assert(next == state)
  }

  test("Drag clamps to MinSizeRatio so panes can't collapse") {
    val armed = SplitPane.DragState(0.5, dragging = true)
    val drag0 = MouseEvent.Drag(MouseButton.Left, col = 1, row = 3, mods)
    val next0 = SplitPane.handleMouse(armed, drag0, SplitPane.Direction.Horizontal, 20, 5, gap = 1)
    // While dragging, ratio is the raw cursor ratio — clamp happens on Release.
    val release0 = MouseEvent.Release(MouseButton.Left, col = 1, row = 3, mods)
    val final0   = SplitPane.handleMouse(next0, release0, SplitPane.Direction.Horizontal, 20, 5, gap = 1)
    assert(final0.splitRatio >= SplitPane.MinSizeRatio)
  }

  test("Vertical split uses row position for the ratio") {
    val armed = SplitPane.DragState(0.5, dragging = true)
    val ev    = MouseEvent.Drag(MouseButton.Left, col = 5, row = 8, mods)
    val next  = SplitPane.handleMouse(armed, ev, SplitPane.Direction.Vertical, 10, 16, gap = 1)
    // rel = (8 - 1) = 7 of 15 (= height - 1) → 7/15
    assert(math.abs(next.splitRatio - 7.0 / 15.0) < 1e-6, s"expected ~${7.0 / 15.0}, got ${next.splitRatio}")
  }

  // ---- drag symmetry (regression for llm4s/termflow#208) ------------------

  test("far-left and far-right horizontal drags are mirror images about 0.5") {
    // The divider should reach both ends symmetrically: a drag d cells in
    // from the left and the matching drag d cells in from the right must map
    // to ratios that sum to 1 (mirror about the centre).
    for
      width <- Seq(2, 5, 10, 20, 21)
      d     <- 0 to (width - 1) / 2
    do
      val left  = MouseEvent.Drag(MouseButton.Left, col = 1 + d, row = 3, mods)
      val right = MouseEvent.Drag(MouseButton.Left, col = 1 + (width - 1 - d), row = 3, mods)
      val armed = SplitPane.DragState(0.5, dragging = true)
      val lr    = SplitPane.handleMouse(armed, left, SplitPane.Direction.Horizontal, width, 5, gap = 1).splitRatio
      val rr    = SplitPane.handleMouse(armed, right, SplitPane.Direction.Horizontal, width, 5, gap = 1).splitRatio
      assert(math.abs((lr + rr) - 1.0) < 1e-9, s"not symmetric at width=$width d=$d: $lr + $rr")
  }

  test("far-left/far-right drags reach the symmetric raw endpoints 0.0 and 1.0") {
    val armed   = SplitPane.DragState(0.5, dragging = true)
    val width   = 20
    val leftEv  = MouseEvent.Drag(MouseButton.Left, col = 1, row = 3, mods)             // far left
    val rightEv = MouseEvent.Drag(MouseButton.Left, col = 1 + width - 1, row = 3, mods) // far right
    val l       = SplitPane.handleMouse(armed, leftEv, SplitPane.Direction.Horizontal, width, 5, gap = 1).splitRatio
    val r       = SplitPane.handleMouse(armed, rightEv, SplitPane.Direction.Horizontal, width, 5, gap = 1).splitRatio
    assert(math.abs(l - 0.0) < 1e-9, s"far-left raw ratio should be 0.0, got $l")
    assert(math.abs(r - 1.0) < 1e-9, s"far-right raw ratio should be 1.0, got $r")
  }
