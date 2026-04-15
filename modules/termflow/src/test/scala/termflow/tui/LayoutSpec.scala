package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.Layout.asLayout
import termflow.tui.TuiPrelude.*

class LayoutSpec extends AnyFunSuite:

  private def tn(text: String): TextNode =
    TextNode(1.x, 1.y, List(Text(text, Style())))

  // --- measure -----------------------------------------------------------

  test("measureVNode on TextNode returns (textWidth, 1)"):
    assert(Layout.measureVNode(tn("abc")) == (3, 1))
    assert(Layout.measureVNode(tn("")) == (0, 1))
    // Multi-segment text sums the widths.
    val multi = TextNode(1.x, 1.y, List(Text("ab", Style()), Text("cde", Style())))
    assert(Layout.measureVNode(multi) == (5, 1))

  test("measureVNode on BoxNode returns declared dimensions"):
    assert(Layout.measureVNode(BoxNode(1.x, 1.y, 10, 4, children = Nil)) == (10, 4))

  test("measureVNode on InputNode uses lineWidth when set, prompt length otherwise"):
    val boxed = InputNode(1.x, 1.y, "hello", Style(), lineWidth = 20)
    assert(Layout.measureVNode(boxed) == (20, 1))
    val unboxed = InputNode(1.x, 1.y, "hello", Style(), lineWidth = 0)
    assert(Layout.measureVNode(unboxed) == (5, 1))

  test("Elem measures to its wrapped VNode"):
    assert(Layout.Elem(tn("hi")).measure == (2, 1))

  test("Spacer measures to its declared dimensions"):
    assert(Layout.Spacer(3, 2).measure == (3, 2))
    // Negative sizes clamp to zero.
    assert(Layout.Spacer(-5, -1).measure == (0, 0))

  test("empty Row and Column measure to (0, 0)"):
    assert(Layout.Row(gap = 2, children = Nil).measure == (0, 0))
    assert(Layout.Column(gap = 2, children = Nil).measure == (0, 0))

  test("Row measure sums widths and picks max height, with gap between siblings"):
    val r = Layout.Row(
      gap = 1,
      children = List(
        Layout.Elem(tn("ab")),                               // 2x1
        Layout.Elem(tn("cdef")),                             // 4x1
        Layout.Elem(BoxNode(1.x, 1.y, 3, 2, children = Nil)) // 3x2
      )
    )
    // width = 2 + 4 + 3 + 1*(3-1) = 11, height = max(1,1,2) = 2
    assert(r.measure == (11, 2))

  test("Column measure sums heights and picks max width, with gap between siblings"):
    val c = Layout.Column(
      gap = 2,
      children = List(
        Layout.Elem(tn("ab")),                                // 2x1
        Layout.Elem(BoxNode(1.x, 1.y, 5, 3, children = Nil)), // 5x3
        Layout.Elem(tn("xy"))                                 // 2x1
      )
    )
    // width = max(2,5,2) = 5, height = 1 + 3 + 1 + 2*(3-1) = 9
    assert(c.measure == (5, 9))

  test("single-child container emits no gap"):
    assert(Layout.Row(gap = 10, children = List(tn("abc").asLayout)).measure == (3, 1))
    assert(Layout.Column(gap = 10, children = List(tn("abc").asLayout)).measure == (3, 1))

  test("negative gap is clamped to zero"):
    val r = Layout.Row(gap = -5, children = List(tn("ab").asLayout, tn("cd").asLayout))
    assert(r.measure == (4, 1))

  // --- resolve -----------------------------------------------------------

  test("Elem.resolve shifts an authored-at-(1,1) node to the requested origin"):
    val placed = Layout.Elem(tn("AB")).resolve(Coord(5.x, 3.y))
    assert(placed.size == 1)
    assert(placed.head.x.value == 5)
    assert(placed.head.y.value == 3)

  test("Elem.resolve at (1,1) is an identity for nodes authored at (1,1)"):
    val node   = tn("hi")
    val placed = Layout.Elem(node).resolve(Coord(1.x, 1.y))
    assert(placed == List(node))

  test("Elem.resolve translates a node authored at non-(1,1) to the container slot"):
    val node   = TextNode(3.x, 2.y, List(Text("X", Style())))
    val placed = Layout.Elem(node).resolve(Coord(7.x, 5.y))
    // dx = 7-3 = 4, dy = 5-2 = 3 => landed at (7, 5)
    assert(placed.head.x.value == 7)
    assert(placed.head.y.value == 5)

  test("Elem.resolve preserves BoxNode child offsets when translating"):
    val inner  = TextNode(11.x, 6.y, List(Text("i", Style())))
    val box    = BoxNode(10.x, 5.y, 4, 2, children = List(inner))
    val placed = Layout.Elem(box).resolve(Coord(1.x, 1.y))
    assert(placed.size == 1)
    val p = placed.head.asInstanceOf[BoxNode]
    assert(p.x.value == 1 && p.y.value == 1)
    // Inner was at (11, 6), offset (+1, +1) from box. After translation it
    // should still be (+1, +1) from the new box origin => (2, 2).
    val pi = p.children.head
    assert(pi.x.value == 2 && pi.y.value == 2)

  test("Row.resolve places children left-to-right with gap"):
    val r = Layout.Row(
      gap = 1,
      children = List(
        tn("AB").asLayout,
        tn("CD").asLayout,
        tn("E").asLayout
      )
    )
    val placed = r.resolve(Coord(1.x, 1.y))
    assert(placed.size == 3)
    // AB at 1..2, gap at 3, CD at 4..5, gap at 6, E at 7
    assert(placed(0).x.value == 1)
    assert(placed(1).x.value == 4)
    assert(placed(2).x.value == 7)
    assert(placed.forall(_.y.value == 1))

  test("Column.resolve places children top-to-bottom with gap"):
    val c = Layout.Column(
      gap = 2,
      children = List(
        tn("first").asLayout,
        tn("second").asLayout,
        tn("third").asLayout
      )
    )
    val placed = c.resolve(Coord(4.x, 2.y))
    // rows are 1 tall each, gap of 2 between them
    assert(placed(0).y.value == 2)
    assert(placed(1).y.value == 5)
    assert(placed(2).y.value == 8)
    assert(placed.forall(_.x.value == 4))

  test("Spacer.resolve produces no VNodes but Row/Column still account for its width"):
    val r = Layout.Row(
      gap = 0,
      children = List(
        tn("A").asLayout,
        Layout.Spacer(3, 1),
        tn("B").asLayout
      )
    )
    val placed = r.resolve(Coord(1.x, 1.y))
    assert(placed.size == 2)
    // A at 1, spacer at 2..4 (not rendered), B at 5
    assert(placed(0).x.value == 1)
    assert(placed(1).x.value == 5)

  test("nested Row inside Column resolves each container at its slot"):
    val layout = Layout.Column(
      gap = 0,
      children = List(
        tn("top").asLayout,
        Layout.Row(
          gap = 1,
          children = List(
            tn("L").asLayout,
            tn("R").asLayout
          )
        )
      )
    )
    val placed = layout.resolve(Coord(1.x, 1.y))
    assert(placed.size == 3)
    // top at (1,1)
    assert(placed(0).x.value == 1 && placed(0).y.value == 1)
    // L at (1,2) — row starts at y=2, x=1
    assert(placed(1).x.value == 1 && placed(1).y.value == 2)
    // R at (3,2) — L is 1 wide + 1 gap
    assert(placed(2).x.value == 3 && placed(2).y.value == 2)

  test("empty Row/Column resolve to the empty list"):
    assert(Layout.Row(gap = 3, children = Nil).resolve(Coord(1.x, 1.y)).isEmpty)
    assert(Layout.Column(gap = 3, children = Nil).resolve(Coord(1.x, 1.y)).isEmpty)

  // --- fluent constructors ----------------------------------------------

  test("Layout.row and Layout.column auto-wrap VNodes"):
    val r = Layout.row(gap = 1)(tn("A"), tn("B"))
    assert(r.measure == (3, 1))

    val c = Layout.column(gap = 0)(tn("A"), tn("B"))
    assert(c.measure == (1, 2))

  test("Layout.row clamps negative gap to zero"):
    val r = Layout.row(gap = -1)(tn("A"), tn("B"))
    assert(r.measure == (2, 1))

  // --- toRootNode -------------------------------------------------------

  test("toRootNode wraps resolve output in a RootNode with the given dimensions"):
    val layout = Layout.row(gap = 1)(tn("A"), tn("B"))
    val root   = layout.toRootNode(width = 40, height = 3)
    assert(root.width == 40 && root.height == 3)
    assert(root.children.size == 2)
    assert(root.input.isEmpty)

  // --- end-to-end integration -------------------------------------------

  test("a resolved layout renders as expected through AnsiRenderer.buildFrame"):
    // Build a Row("AB", "CD") at (1,1), render, assert cell contents.
    val root                   = Layout.row(gap = 1)(tn("AB"), tn("CD")).toRootNode(width = 10, height = 1)
    val frame                  = AnsiRenderer.buildFrame(root)
    def charAt(x: Int, y: Int) = frame.cells(y - 1)(x - 1).ch
    assert(charAt(1, 1) == 'A')
    assert(charAt(2, 1) == 'B')
    assert(charAt(3, 1) == ' ') // gap
    assert(charAt(4, 1) == 'C')
    assert(charAt(5, 1) == 'D')

  test("a nested Column containing a Row renders at the right coordinates"):
    val root = Layout.column(gap = 0)(
      tn("top")
    ) match
      case Layout.Column(g, cs) =>
        // Compose via ADT to include a Row child.
        val full = Layout.Column(
          g,
          cs ++ List(
            Layout.Row(gap = 1, children = List(tn("L").asLayout, tn("R").asLayout))
          )
        )
        full.toRootNode(width = 10, height = 2)
      case _ => fail("expected a Column")

    val frame = AnsiRenderer.buildFrame(root)
    // top: row 1, cols 1..3
    assert(frame.cells(0)(0).ch == 't')
    assert(frame.cells(0)(1).ch == 'o')
    assert(frame.cells(0)(2).ch == 'p')
    // row 2: L at col 1, gap at col 2, R at col 3
    assert(frame.cells(1)(0).ch == 'L')
    assert(frame.cells(1)(1).ch == ' ')
    assert(frame.cells(1)(2).ch == 'R')
