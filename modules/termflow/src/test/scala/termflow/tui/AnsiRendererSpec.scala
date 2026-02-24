package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class AnsiRendererSpec extends AnyFunSuite:

  test("moveTo generates correct ANSI escape sequence"):
    assert(AnsiRenderer.moveTo(XCoord(1), YCoord(1)) == "\u001b[1;1H")
    assert(AnsiRenderer.moveTo(XCoord(10), YCoord(5)) == "\u001b[5;10H")
    assert(AnsiRenderer.moveTo(XCoord(80), YCoord(24)) == "\u001b[24;80H")

  test("moveTo with Coord generates correct ANSI escape sequence"):
    assert(AnsiRenderer.moveTo(Coord(XCoord(1), YCoord(1))) == "\u001b[1;1H")
    assert(AnsiRenderer.moveTo(Coord(XCoord(10), YCoord(5))) == "\u001b[5;10H")

  test("XCoord arithmetic works correctly"):
    val x = XCoord(5)
    assert((x + 3).value == 8)
    assert((x - 2).value == 3)

  test("YCoord arithmetic works correctly"):
    val y = YCoord(10)
    assert((y + 5).value == 15)
    assert((y - 3).value == 7)

  test("Color ordinal values are correct"):
    assert(Color.Default.ordinal == 0)
    assert(Color.Black.ordinal == 1)
    assert(Color.Red.ordinal == 2)
    assert(Color.Green.ordinal == 3)
    assert(Color.Yellow.ordinal == 4)
    assert(Color.Blue.ordinal == 5)
    assert(Color.Magenta.ordinal == 6)
    assert(Color.Cyan.ordinal == 7)
    assert(Color.White.ordinal == 8)

  test("Style defaults are correct"):
    val style = Style()
    assert(style.fg == Color.Default)
    assert(style.bg == Color.Default)
    assert(!style.bold)
    assert(!style.underline)
    assert(!style.border)

  test("ANSI constants are correctly defined"):
    assert(ANSI.saveCursor == "\u001b[s")
    assert(ANSI.restoreCursor == "\u001b[u")
    assert(ANSI.enterAltBuffer == "\u001b[?1049h")
    assert(ANSI.exitAltBuffer == "\u001b[?1049l")
    assert(ANSI.clearScreen == "\u001b[2J")
    assert(ANSI.homeCursor == "\u001b[H")
    assert(ANSI.hideCursor == "\u001b[?25l")
    assert(ANSI.showCursor == "\u001b[?25h")

  test("InputNode width uses lineWidth when specified"):
    val inp1 = InputNode(XCoord(1), YCoord(1), "hello", Style())
    assert(inp1.width == 6) // prompt.length + 1

    val inp2 = InputNode(XCoord(1), YCoord(1), "hello", Style(), lineWidth = 80)
    assert(inp2.width == 80)

  test("TextNode default dimensions"):
    val node = TextNode(XCoord(1), YCoord(1), List(Text("hello", Style())))
    assert(node.width == 1)
    assert(node.height == 1)
    assert(node.style == Style())
