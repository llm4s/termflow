package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class RadioGroupSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private def textsOf(nodes: List[VNode]): List[String] = nodes.map {
    case TextNode(_, _, runs) => runs.map(_.txt).mkString
    case other                => fail(s"expected TextNode, got $other")
  }

  test("renders one row per option with correct selected/unselected markers") {
    val nodes = RadioGroup(Seq("Light", "Dark", "Mono"), selectedIndex = 1)
    val texts = textsOf(nodes)
    assert(texts.length == 3)
    assert(texts(0).contains("○") && texts(0).contains("Light"))
    assert(texts(1).contains("◉") && texts(1).contains("Dark"))
    assert(texts(2).contains("○") && texts(2).contains("Mono"))
  }

  test("ASCII fallback uses (*) and ( ) markers") {
    val nodes = RadioGroup(Seq("On", "Off"), selectedIndex = 0, unicode = false)
    val texts = textsOf(nodes)
    assert(texts(0).contains("(*)"))
    assert(texts(1).contains("( )"))
  }

  test("focusedIndex bolds only that row") {
    val nodes = RadioGroup(Seq("a", "b", "c"), selectedIndex = 0, focusedIndex = 2)
    val rows = nodes.map {
      case TextNode(_, _, runs) => runs.last.style.bold
      case _                    => fail("expected TextNode")
    }
    assert(rows == List(false, false, true))
  }

  test("focusedIndex == -1 means no row is focused") {
    val nodes = RadioGroup(Seq("a", "b"), selectedIndex = 0, focusedIndex = -1)
    nodes.foreach {
      case TextNode(_, _, runs) => assert(!runs.last.style.bold)
      case _                    => fail("expected TextNode")
    }
  }

  test("rows are positioned vertically starting at `at` and one row apart") {
    val nodes = RadioGroup(Seq("a", "b", "c"), selectedIndex = 0, at = Coord(4.x, 7.y))
    val ys = nodes.map {
      case TextNode(_, y, _) => y.value
      case _                 => fail("expected TextNode")
    }
    val xs = nodes.map {
      case TextNode(x, _, _) => x.value
      case _                 => fail("expected TextNode")
    }
    assert(ys == List(7, 8, 9))
    assert(xs.forall(_ == 4))
  }

  test("RadioGroup.glyph maps every (selected, unicode) combo") {
    assert(RadioGroup.glyph(selected = true) == "◉")
    assert(RadioGroup.glyph(selected = false) == "○")
    assert(RadioGroup.glyph(selected = true, unicode = false) == "(*)")
    assert(RadioGroup.glyph(selected = false, unicode = false) == "( )")
  }

  test("RadioGroup.width is marker + space + longest label") {
    assert(RadioGroup.width(Seq("ab", "xyz", "x")) == 1 + 1 + 3)     // ◉ + ' ' + 'xyz'
    assert(RadioGroup.width(Seq("a"), unicode = false) == 3 + 1 + 1) // (*) + ' ' + 'a'
    assert(RadioGroup.width(Seq.empty[String]) == 0, "empty group has zero width")
  }

  test("RadioGroup.height equals the option count") {
    assert(RadioGroup.height(Seq("a", "b", "c")) == 3)
    assert(RadioGroup.height(Seq.empty[String]) == 0)
  }

  test("empty options produces an empty list") {
    val nodes = RadioGroup(Seq.empty[String], selectedIndex = 0)
    assert(nodes.isEmpty)
  }
