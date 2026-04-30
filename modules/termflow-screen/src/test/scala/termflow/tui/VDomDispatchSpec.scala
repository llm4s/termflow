package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

/**
 * Coverage-oriented exercises that drive the [[VNode.x]] / [[VNode.y]] /
 * [[VNode.width]] / [[VNode.height]] / [[VNode.style]] dispatch and the
 * private `Color.toRgb` helper. These paths are hit when a value's static
 * type is `VNode` (or `Color`) rather than the concrete case.
 */
class VDomDispatchSpec extends AnyFunSuite:

  private val text: VNode = TextNode(XCoord(2), YCoord(3), List(Text("abc", Style())))
  private val box: VNode  = BoxNode(XCoord(4), YCoord(5), 7, 8, children = Nil, style = Style(border = true))
  private val inp: VNode  = InputNode(XCoord(6), YCoord(7), "p", Style(fg = Color.Red), lineWidth = 12)

  test("VNode.x dispatches to each case"):
    assert(text.x == XCoord(2))
    assert(box.x == XCoord(4))
    assert(inp.x == XCoord(6))

  test("VNode.y dispatches to each case"):
    assert(text.y == YCoord(3))
    assert(box.y == YCoord(5))
    assert(inp.y == YCoord(7))

  test("VNode.width dispatches to each case"):
    assert(text.width == 1)
    assert(box.width == 7)
    assert(inp.width == 12)

  test("VNode.height dispatches to each case"):
    assert(text.height == 1)
    assert(box.height == 8)
    assert(inp.height == 1)

  test("VNode.style dispatches to each case"):
    assert(text.style == Style())
    assert(box.style == Style(border = true))
    assert(inp.style == Style(fg = Color.Red))

  test("InputNode width falls back to prompt.length + 1 when lineWidth is 0"):
    val v: VNode = InputNode(XCoord(1), YCoord(1), "abcd", Style(), lineWidth = 0)
    assert(v.width == 5)

  // ---- Color.toRgb -------------------------------------------------------

  test("Color.toRgb returns None for Default"):
    assert(Color.toRgb(Color.Default).isEmpty)

  test("Color.toRgb returns the named-palette entry for a basic color"):
    assert(Color.toRgb(Color.Red).contains((170, 0, 0)))
    assert(Color.toRgb(Color.BrightWhite).contains((255, 255, 255)))

  test("Color.toRgb resolves Indexed via the palette table"):
    assert(Color.toRgb(Color.Indexed(0)).contains((0, 0, 0)))
    val cube = Color.toRgb(Color.Indexed(196))
    assert(cube.contains((255, 0, 0)))

  test("Color.toRgb clamps Indexed inputs to 0..255"):
    assert(Color.toRgb(Color.Indexed(-5)) == Color.toRgb(Color.Indexed(0)))
    assert(Color.toRgb(Color.Indexed(999)) == Color.toRgb(Color.Indexed(255)))

  test("Color.toRgb returns the Rgb triple, clamping components"):
    assert(Color.toRgb(Color.Rgb(10, 20, 30)).contains((10, 20, 30)))
    assert(Color.toRgb(Color.Rgb(-1, 300, 128)).contains((0, 255, 128)))
