package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*

class SeparatorSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private def textOf(node: VNode): String = node match
    case t: TextNode => t.txt.map(_.txt).mkString
    case _           => ""

  test("horizontal renders one TextNode width cells long"):
    val nodes = Separator.horizontal(width = 10)
    assert(nodes.size == 1)
    val s = textOf(nodes.head)
    assert(s.length == 10, s"expected 10 cells, got '${s}'")

  test("horizontal with non-positive width yields no nodes"):
    assert(Separator.horizontal(width = 0).isEmpty)
    assert(Separator.horizontal(width = -3).isEmpty)

  test("horizontal embeds a centred title with ' Title ' padding"):
    val nodes = Separator.horizontal(width = 20, title = "Header")
    val s     = textOf(nodes.head)
    assert(s.length == 20)
    assert(s.contains(" Header "))
    // Centred — one or two leader cells before " Header " on either side.
    val titleIdx = s.indexOf(" Header ")
    val trailer  = s.length - titleIdx - " Header ".length
    assert(math.abs(titleIdx - trailer) <= 1)

  test("horizontal drops the title when there isn't room for leader + trailer"):
    // " Header " = 8 cells, +2 for one cell of leader and trailer = 10, so 9 too narrow.
    val nodes = Separator.horizontal(width = 9, title = "Header")
    val s     = textOf(nodes.head)
    assert(!s.contains("Header"))
    assert(s.length == 9)

  test("vertical emits one TextNode per row"):
    val nodes = Separator.vertical(height = 5)
    assert(nodes.size == 5)
    nodes.zipWithIndex.foreach { case (n, i) =>
      assert(n.y.value == 1 + i, s"row $i should be at y=${1 + i}, got ${n.y.value}")
    }

  test("vertical with non-positive height yields no nodes"):
    assert(Separator.vertical(height = 0).isEmpty)
    assert(Separator.vertical(height = -2).isEmpty)
