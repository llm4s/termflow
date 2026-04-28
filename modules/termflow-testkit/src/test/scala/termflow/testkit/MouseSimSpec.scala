package termflow.testkit

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.KeyDecoder.Modifiers
import termflow.tui.MouseButton
import termflow.tui.MouseEvent
import termflow.tui.ScrollDirection

class MouseSimSpec extends AnyFunSuite:

  test("click returns a left-button Press wrapped in InputKey.Mouse") {
    val k = MouseSim.click(7, 3)
    k match
      case InputKey.Mouse(MouseEvent.Press(MouseButton.Left, c, r, mods)) =>
        assert(c == 7 && r == 3)
        assert(mods.isEmpty)
      case other => fail(s"expected left-press at (7,3), got $other")
  }

  test("press preserves the requested button + modifiers") {
    val k = MouseSim.press(2, 4, MouseButton.Right, Modifiers(ctrl = true))
    k match
      case InputKey.Mouse(MouseEvent.Press(MouseButton.Right, 2, 4, mods)) =>
        assert(mods.ctrl)
        assert(!mods.shift)
      case other => fail(s"unexpected $other")
  }

  test("release builds a release event") {
    val k = MouseSim.release(10, 5)
    k match
      case InputKey.Mouse(MouseEvent.Release(MouseButton.Left, 10, 5, _)) => ()
      case other                                                          => fail(s"unexpected $other")
  }

  test("drag builds a drag event") {
    val k = MouseSim.drag(11, 6, MouseButton.Middle)
    k match
      case InputKey.Mouse(MouseEvent.Drag(MouseButton.Middle, 11, 6, _)) => ()
      case other                                                         => fail(s"unexpected $other")
  }

  test("move builds a move event") {
    val k = MouseSim.move(12, 7)
    k match
      case InputKey.Mouse(MouseEvent.Move(12, 7, _)) => ()
      case other                                     => fail(s"unexpected $other")
  }

  test("scrollOnce wraps a single Scroll event in the requested direction") {
    val k = MouseSim.scrollOnce(ScrollDirection.Up, 4, 4)
    k match
      case InputKey.Mouse(MouseEvent.Scroll(ScrollDirection.Up, 4, 4, _)) => ()
      case other                                                          => fail(s"unexpected $other")
  }

  test("clickPair returns press then release with matching coordinates") {
    val pair = MouseSim.clickPair(2, 3, MouseButton.Left)
    assert(pair.size == 2)
    pair(0) match
      case InputKey.Mouse(MouseEvent.Press(MouseButton.Left, 2, 3, _)) => ()
      case other                                                       => fail(s"expected press first, got $other")
    pair(1) match
      case InputKey.Mouse(MouseEvent.Release(MouseButton.Left, 2, 3, _)) => ()
      case other                                                         => fail(s"expected release second, got $other")
  }

  test("scroll repeats a single direction for the requested ticks") {
    val ks = MouseSim.scroll(ScrollDirection.Down, 5, 5, ticks = 3)
    assert(ks.size == 3)
    ks.foreach {
      case InputKey.Mouse(MouseEvent.Scroll(ScrollDirection.Down, 5, 5, _)) => ()
      case other                                                            => fail(s"unexpected $other")
    }
  }

  test("scroll with ticks = 0 yields an empty sequence") {
    assert(MouseSim.scroll(ScrollDirection.Up, 1, 1, ticks = 0).isEmpty)
  }

  test("scroll rejects negative ticks") {
    intercept[IllegalArgumentException](MouseSim.scroll(ScrollDirection.Up, 1, 1, ticks = -1))
  }

  test("scrollUp / scrollDown / scrollLeft / scrollRight pick the right direction") {
    def dirOf(k: InputKey): ScrollDirection = k match
      case InputKey.Mouse(MouseEvent.Scroll(d, _, _, _)) => d
      case other                                         => fail(s"not a scroll event: $other")

    assert(MouseSim.scrollUp(1, 1).map(dirOf).head == ScrollDirection.Up)
    assert(MouseSim.scrollDown(1, 1).map(dirOf).head == ScrollDirection.Down)
    assert(MouseSim.scrollLeft(1, 1).map(dirOf).head == ScrollDirection.Left)
    assert(MouseSim.scrollRight(1, 1).map(dirOf).head == ScrollDirection.Right)
  }

  test("dragGesture starts with a press, ends with a release at the destination") {
    val seq = MouseSim.dragGesture(2, 2, 8, 4, steps = 2)
    seq.head match
      case InputKey.Mouse(MouseEvent.Press(_, 2, 2, _)) => ()
      case other                                        => fail(s"first event should be press at origin, got $other")
    seq.last match
      case InputKey.Mouse(MouseEvent.Release(_, 8, 4, _)) => ()
      case other => fail(s"last event should be release at destination, got $other")
  }

  test("dragGesture with steps = 0 collapses to press + drag-to-end + release") {
    val seq = MouseSim.dragGesture(0, 0, 5, 5, steps = 0)
    assert(seq.size == 3)
    seq(0) match
      case InputKey.Mouse(MouseEvent.Press(_, 0, 0, _)) => ()
      case other                                        => fail(s"expected press, got $other")
    seq(1) match
      case InputKey.Mouse(MouseEvent.Drag(_, 5, 5, _)) => ()
      case other                                       => fail(s"expected drag-to-end, got $other")
    seq(2) match
      case InputKey.Mouse(MouseEvent.Release(_, 5, 5, _)) => ()
      case other                                          => fail(s"expected release, got $other")
  }

  test("dragGesture samples intermediate drag positions on the line") {
    val seq   = MouseSim.dragGesture(0, 0, 10, 0, steps = 4)
    val drags = seq.collect { case InputKey.Mouse(d @ MouseEvent.Drag(_, _, _, _)) => d }
    // 4 intermediate samples + final drag at the destination = 5 drag events.
    assert(drags.size == 5)
    // X-coordinate must be non-decreasing along the sampled drags.
    val xs = drags.map(_.col)
    assert(xs == xs.sorted)
    // Final drag is exactly at the destination.
    assert(drags.last.col == 10 && drags.last.row == 0)
  }

  test("dragGesture rejects negative steps") {
    intercept[IllegalArgumentException](
      MouseSim.dragGesture(0, 0, 1, 1, steps = -1)
    )
  }
