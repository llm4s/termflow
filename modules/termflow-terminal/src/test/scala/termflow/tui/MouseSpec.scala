package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class MouseSpec extends AnyFunSuite:

  private val noMods = KeyDecoder.Modifiers()

  test("MouseEvent.at returns the (col, row) of every event variant"):
    val cases: List[(MouseEvent, (Int, Int))] = List(
      (MouseEvent.Press(MouseButton.Left, 3, 4, noMods), (3, 4)),
      (MouseEvent.Release(MouseButton.Right, 5, 6, noMods), (5, 6)),
      (MouseEvent.Drag(MouseButton.Middle, 7, 8, noMods), (7, 8)),
      (MouseEvent.Move(9, 10, noMods), (9, 10)),
      (MouseEvent.Scroll(ScrollDirection.Up, 11, 12, noMods), (11, 12))
    )
    cases.foreach { case (ev, expected) => assert(ev.at == expected) }

  test("MouseEvent.modifiers returns the modifier set of every variant"):
    val mods = KeyDecoder.Modifiers(shift = true, alt = true)
    val cases: List[MouseEvent] = List(
      MouseEvent.Press(MouseButton.Left, 1, 1, mods),
      MouseEvent.Release(MouseButton.Right, 1, 1, mods),
      MouseEvent.Drag(MouseButton.Middle, 1, 1, mods),
      MouseEvent.Move(1, 1, mods),
      MouseEvent.Scroll(ScrollDirection.Down, 1, 1, mods)
    )
    cases.foreach(ev => assert(ev.modifiers == mods))

  test("fromSgr decodes Middle button press"):
    assert(
      MouseEvent
        .fromSgr(button = 1, col = 5, row = 6, releaseFinal = false)
        .contains(MouseEvent.Press(MouseButton.Middle, 5, 6, noMods))
    )

  test("fromSgr decodes scroll Left and Right wheel ticks"):
    assert(
      MouseEvent
        .fromSgr(button = 0x40 | 2, col = 1, row = 1, releaseFinal = false)
        .contains(MouseEvent.Scroll(ScrollDirection.Left, 1, 1, noMods))
    )
    assert(
      MouseEvent
        .fromSgr(button = 0x40 | 3, col = 1, row = 1, releaseFinal = false)
        .contains(MouseEvent.Scroll(ScrollDirection.Right, 1, 1, noMods))
    )

  test("fromSgr returns None for the duplicate scroll-release byte"):
    assert(
      MouseEvent.fromSgr(button = 0x40 | 0, col = 1, row = 1, releaseFinal = true).isEmpty
    )

  test("fromSgr decodes button-8 / button-9 extra mouse buttons"):
    val ev8 = MouseEvent.fromSgr(button = 0x80 | 0, col = 1, row = 1, releaseFinal = false).get
    val ev9 = MouseEvent.fromSgr(button = 0x80 | 1, col = 1, row = 1, releaseFinal = false).get
    ev8 match
      case MouseEvent.Press(MouseButton.Other(8), _, _, _) => ()
      case other                                           => fail(s"expected Other(8), got $other")
    ev9 match
      case MouseEvent.Press(MouseButton.Other(9), _, _, _) => ()
      case other                                           => fail(s"expected Other(9), got $other")

  test("fromSgr maps motion with no held button to Move"):
    val ev = MouseEvent.fromSgr(button = 0x20 | 3, col = 4, row = 5, releaseFinal = false)
    assert(ev.contains(MouseEvent.Move(4, 5, noMods)))

  test("fromSgr maps motion with a held middle button to Drag"):
    val ev = MouseEvent.fromSgr(button = 0x20 | 1, col = 4, row = 5, releaseFinal = false)
    assert(ev.contains(MouseEvent.Drag(MouseButton.Middle, 4, 5, noMods)))
