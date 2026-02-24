package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class VDomSpec extends AnyFunSuite:

  test("TextNode exposes position and default dimensions/style"):
    val node = TextNode(XCoord(2), YCoord(3), List(Text("hello", Style(fg = Color.Red))))

    assert(node.x == XCoord(2))
    assert(node.y == YCoord(3))
    assert(node.width == 1)
    assert(node.height == 1)
    assert(node.style == Style())

  test("BoxNode exposes configured geometry/style/children"):
    val child = TextNode(XCoord(4), YCoord(5), List(Text("child", Style())))
    val node = BoxNode(
      x = XCoord(1),
      y = YCoord(2),
      width = 40,
      height = 10,
      children = List(child),
      style = Style(fg = Color.Blue, border = true)
    )

    assert(node.x == XCoord(1))
    assert(node.y == YCoord(2))
    assert(node.width == 40)
    assert(node.height == 10)
    assert(node.style == Style(fg = Color.Blue, border = true))
    node match
      case VNode.BoxNode(_, _, _, _, children, _) => assert(children == List(child))
      case _                                      => fail("expected BoxNode")

  test("InputNode computes width from prompt or lineWidth override"):
    val nodeByPrompt = InputNode(
      x = XCoord(10),
      y = YCoord(11),
      prompt = "abc",
      style = Style(fg = Color.Green),
      cursor = 1
    )
    val nodeByLineWidth = InputNode(
      x = XCoord(10),
      y = YCoord(11),
      prompt = "abc",
      style = Style(fg = Color.Green),
      cursor = 1,
      lineWidth = 20
    )

    assert(nodeByPrompt.x == XCoord(10))
    assert(nodeByPrompt.y == YCoord(11))
    assert(nodeByPrompt.height == 1)
    assert(nodeByPrompt.width == 4)
    assert(nodeByPrompt.style == Style(fg = Color.Green))
    nodeByPrompt match
      case VNode.InputNode(_, _, prompt, _, cursor, _) =>
        assert(prompt == "abc")
        assert(cursor == 1)
      case _ => fail("expected InputNode")

    assert(nodeByLineWidth.width == 20)
