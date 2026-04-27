package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.TuiPrelude.*

class LayoutFillSpec extends AnyFunSuite:

  // ---- measure --------------------------------------------------------------

  test("Layout.Fill measure delegates to inner") {
    val inner = TextNode(1.x, 1.y, List(Text("hello", Style())))
    assert(Layout.measure(Layout.Fill(Layout.Elem(inner))) == (5, 1))
  }

  // ---- distributeMajor / resolveTo: Row -------------------------------------

  test("Row without Fill resolves at natural sizes regardless of available width") {
    val l = Layout.row(gap = 1)(
      TextNode(1.x, 1.y, List(Text("ab", Style()))),
      TextNode(1.x, 1.y, List(Text("c", Style())))
    )
    val out = Layout.resolveTo(l, Coord(1.x, 1.y), availableWidth = 80, availableHeight = 1)
    val xs  = out.collect { case t: VNode.TextNode => t.x.value }
    assert(xs == List(1, 4)) // "ab" at 1, gap 1, "c" at 4
  }

  test("Row with one Fill child consumes the remaining axis width") {
    val l = Layout.Row(
      gap = 0,
      children = List(
        Layout.Elem(TextNode(1.x, 1.y, List(Text("AA", Style())))), // width 2
        Layout.Fill(Layout.Elem(BoxNode(1.x, 1.y, 1, 1, children = Nil))),
        Layout.Elem(TextNode(1.x, 1.y, List(Text("BB", Style())))) // width 2
      )
    )
    val out = Layout.resolveTo(l, Coord(1.x, 1.y), availableWidth = 20, availableHeight = 1)
    // "AA" at column 1, Fill box at column 3 with width 16, "BB" at column 19.
    val tail = out.collect { case t: VNode.TextNode => (t.x.value, t.txt.head.txt) }
    val box  = out.collect { case b: VNode.BoxNode => (b.x.value, b.width) }
    assert(tail == List((1, "AA"), (19, "BB")), s"text positions wrong: $tail")
    assert(box == List((3, 16)), s"fill box: $box")
  }

  test("multiple Fill siblings split the remaining width evenly") {
    val l = Layout.Row(
      gap = 0,
      children = List(
        Layout.Fill(Layout.Elem(BoxNode(1.x, 1.y, 1, 1, children = Nil))),
        Layout.Fill(Layout.Elem(BoxNode(1.x, 1.y, 1, 1, children = Nil)))
      )
    )
    val out = Layout.resolveTo(l, Coord(1.x, 1.y), availableWidth = 21, availableHeight = 1)
    val ws  = out.collect { case b: VNode.BoxNode => b.width }
    // 21 / 2 = 10, leftover 1 → last gets 11.
    assert(ws == List(10, 11), s"split widths $ws")
  }

  test("Row with no available budget falls back to natural sizes (Fill = inner)") {
    val l = Layout.Row(
      gap = 0,
      children = List(
        Layout.Elem(TextNode(1.x, 1.y, List(Text("AA", Style())))),
        Layout.Fill(Layout.Elem(BoxNode(1.x, 1.y, 4, 1, children = Nil)))
      )
    )
    val out = Layout.resolveTo(l, Coord(1.x, 1.y), availableWidth = -1, availableHeight = -1)
    val box = out.collect { case b: VNode.BoxNode => b.width }
    assert(box == List(4))
  }

  // ---- Column ----------------------------------------------------------------

  test("Column Fill consumes remaining height") {
    val l = Layout.Column(
      gap = 0,
      children = List(
        Layout.Elem(BoxNode(1.x, 1.y, 4, 2, children = Nil)),
        Layout.Fill(Layout.Elem(BoxNode(1.x, 1.y, 4, 1, children = Nil))),
        Layout.Elem(BoxNode(1.x, 1.y, 4, 3, children = Nil))
      )
    )
    val out   = Layout.resolveTo(l, Coord(1.x, 1.y), availableWidth = 4, availableHeight = 12)
    val boxes = out.collect { case b: VNode.BoxNode => (b.y.value, b.height) }
    // First box: y=1, h=2 (natural). Fill: y=3, h=12-2-3=7. Last: y=10, h=3.
    assert(boxes == List((1, 2), (3, 7), (10, 3)), s"box layout: $boxes")
  }

  // ---- RootNode integration -------------------------------------------------

  test("RootNode with no layout still works (defaults to None)") {
    val root = RootNode(80, 24, children = Nil, input = None)
    assert(root.layout.isEmpty)
  }

  test("RootNode.layout is resolved by AnsiRenderer.buildFrame using frame dimensions") {
    val l = Layout.row(gap = 0)(
      TextNode(1.x, 1.y, List(Text("L", Style()))),
      TextNode(1.x, 1.y, List(Text("R", Style())))
    )
    val root  = RootNode(width = 10, height = 1, children = Nil, input = None, layout = Some(l))
    val frame = AnsiRenderer.buildFrame(root)
    val row   = frame.cells(0).map(_.ch).mkString
    // No Fill — children render at their natural positions: "LR" at columns 1-2.
    assert(row.startsWith("LR"), s"row='$row'")
  }

  test("RootNode.layout with Fill expands at render time on the frame width") {
    val l = Layout.Row(
      gap = 0,
      children = List(
        Layout.Elem(TextNode(1.x, 1.y, List(Text("[", Style())))),
        Layout.Fill(Layout.Elem(BoxNode(1.x, 1.y, 1, 1, children = Nil, style = Style(border = false)))),
        Layout.Elem(TextNode(1.x, 1.y, List(Text("]", Style()))))
      )
    )
    val root  = RootNode(width = 10, height = 1, children = Nil, input = None, layout = Some(l))
    val frame = AnsiRenderer.buildFrame(root)
    // "[" at column 1, "]" at column 10 — fill consumed 8 cells in between.
    assert(frame.cells(0)(0).ch == '[')
    assert(frame.cells(0)(9).ch == ']')
  }

  test("RootNode.layout reflows when the frame width changes") {
    val l = Layout.Row(
      gap = 0,
      children = List(
        Layout.Elem(TextNode(1.x, 1.y, List(Text("[", Style())))),
        Layout.Fill(Layout.Elem(BoxNode(1.x, 1.y, 1, 1, children = Nil))),
        Layout.Elem(TextNode(1.x, 1.y, List(Text("]", Style()))))
      )
    )
    val narrow = AnsiRenderer.buildFrame(RootNode(8, 1, Nil, None, layout = Some(l)))
    val wide   = AnsiRenderer.buildFrame(RootNode(20, 1, Nil, None, layout = Some(l)))
    assert(narrow.cells(0)(7).ch == ']')
    assert(wide.cells(0)(19).ch == ']')
  }

  test("RootNode children paint before layout-resolved nodes (layered authoring)") {
    val l        = Layout.row(gap = 0)(TextNode(1.x, 1.y, List(Text("LAYOUT", Style()))))
    val children = List(TextNode(1.x, 1.y, List(Text("BASE", Style()))))
    val root     = RootNode(20, 1, children = children, input = None, layout = Some(l))
    val out      = AnsiRenderer.renderPatch(root)
    val baseIdx  = out.indexOf("BASE")
    val layIdx   = out.indexOf("LAYOUT")
    assert(baseIdx >= 0 && layIdx >= 0)
    assert(layIdx > baseIdx, "layout-resolved nodes must paint after children")
  }
