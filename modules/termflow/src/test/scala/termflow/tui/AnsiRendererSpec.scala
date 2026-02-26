package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream
import java.io.PrintStream

class AnsiRendererSpec extends AnyFunSuite:

  private def captureOut(body: => Unit): String =
    val buf = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(buf))(body)
    buf.toString("UTF-8")

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

  test("render draws bordered boxes, styled text, and input cursor"):
    val root = RootNode(
      width = 80,
      height = 24,
      children = List(
        BoxNode(
          x = XCoord(1),
          y = YCoord(1),
          width = 5,
          height = 3,
          children = List(TextNode(XCoord(2), YCoord(2), List(Text("x", Style(bold = true))))),
          style = Style(fg = Color.Green, border = true)
        )
      ),
      input = Some(InputNode(XCoord(1), YCoord(4), prompt = "ab", style = Style(fg = Color.Red), cursor = 1))
    )

    val out = captureOut(AnsiRenderer.render(root))
    assert(out.contains("┌"))
    assert(out.contains("└"))
    assert(out.contains("\u001b[1m")) // bold style for text
    assert(!out.contains(ANSI.hideCursor))
    assert(out.contains("\u001b[2K"))                               // clear current line
    assert(out.contains(AnsiRenderer.moveTo(XCoord(2), YCoord(4)))) // hardware cursor at logical index

  test("renderInputOnly clamps cursor to end, pads, and positions hardware cursor"):
    val root = RootNode(
      width = 80,
      height = 24,
      children = Nil,
      input = Some(
        InputNode(
          XCoord(10),
          YCoord(5),
          prompt = "abc",
          style = Style(fg = Color.Blue),
          cursor = 999,
          lineWidth = 8
        )
      )
    )

    val out = captureOut(AnsiRenderer.renderInputOnly(root))
    assert(out.contains("\u001b[2K"))
    assert(out.contains(" "))                                        // padded trailing area
    assert(out.contains(AnsiRenderer.moveTo(XCoord(13), YCoord(5)))) // x=10 + clamped cursor len(abc)=3

  test("buildFrame expands to fit rendered extents beyond declared root height"):
    val root = RootNode(
      width = 10,
      height = 2,
      children = List(TextNode(XCoord(1), YCoord(7), List(Text("tail", Style())))),
      input = None
    )

    val frame = AnsiRenderer.buildFrame(root)
    assert(frame.width >= 10)
    assert(frame.height >= 7)

  test("renderDiff clears removed trailing text"):
    val prev = RootNode(
      width = 10,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("abcdef", Style())))),
      input = None
    )
    val curr = RootNode(
      width = 10,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("ab", Style())))),
      input = None
    )

    val ansi = AnsiRenderer.renderDiff(Some(AnsiRenderer.buildFrame(prev)), AnsiRenderer.buildFrame(curr))
    assert(ansi.contains(AnsiRenderer.moveTo(XCoord(3), YCoord(1))))
    assert(ansi.contains("    "))

  test("renderDiff emits no output for identical frame"):
    val root = RootNode(
      width = 10,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("same", Style())))),
      input = Some(InputNode(XCoord(1), YCoord(3), "[]> ", Style(), cursor = 4))
    )

    val frame = AnsiRenderer.buildFrame(root)
    val ansi  = AnsiRenderer.renderDiff(Some(frame), frame)
    assert(ansi.isEmpty)

  test("renderDiff restores cursor when content changes even if cursor position is unchanged"):
    val prev = RootNode(
      width = 20,
      height = 4,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("tick-1", Style())))),
      input = Some(InputNode(XCoord(2), YCoord(4), "[]> ", Style(), cursor = 4))
    )
    val curr = RootNode(
      width = 20,
      height = 4,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("tick-2", Style())))),
      input = Some(InputNode(XCoord(2), YCoord(4), "[]> ", Style(), cursor = 4))
    )

    val ansi = AnsiRenderer.renderDiff(Some(AnsiRenderer.buildFrame(prev)), AnsiRenderer.buildFrame(curr))
    assert(ansi.contains(AnsiRenderer.moveTo(XCoord(6), YCoord(4))))

  test("renderDiff clears removed rows when current frame shrinks"):
    val prev = RootNode(
      width = 12,
      height = 6,
      children = List(TextNode(XCoord(1), YCoord(5), List(Text("footer", Style())))),
      input = None
    )
    val curr = RootNode(
      width = 12,
      height = 3,
      children = Nil,
      input = None
    )

    val ansi = AnsiRenderer.renderDiff(Some(AnsiRenderer.buildFrame(prev)), AnsiRenderer.buildFrame(curr))
    assert(ansi.contains(AnsiRenderer.moveTo(XCoord(1), YCoord(5))))
    assert(ansi.contains("      "))

  test("renderDiff clears stale prompt tail when input text shrinks"):
    val prev = RootNode(
      width = 20,
      height = 5,
      children = Nil,
      input = Some(InputNode(XCoord(1), YCoord(4), "[]> hello", Style(), cursor = 8))
    )
    val curr = RootNode(
      width = 20,
      height = 5,
      children = Nil,
      input = Some(InputNode(XCoord(1), YCoord(4), "[]> hi", Style(), cursor = 6))
    )

    val ansi = AnsiRenderer.renderDiff(Some(AnsiRenderer.buildFrame(prev)), AnsiRenderer.buildFrame(curr))
    assert(ansi.contains(AnsiRenderer.moveTo(XCoord(7), YCoord(4))))
    assert(ansi.contains("   "))

  test("renderDiff does not write past right edge for bordered box updates"):
    val prev = RootNode(
      width = 10,
      height = 4,
      children = List(
        BoxNode(
          x = XCoord(1),
          y = YCoord(1),
          width = 10,
          height = 4,
          children = List(TextNode(XCoord(2), YCoord(2), List(Text("tick-1", Style())))),
          style = Style(fg = Color.Blue, border = true)
        )
      ),
      input = None
    )
    val curr = RootNode(
      width = 10,
      height = 4,
      children = List(
        BoxNode(
          x = XCoord(1),
          y = YCoord(1),
          width = 10,
          height = 4,
          children = List(TextNode(XCoord(2), YCoord(2), List(Text("tick-2", Style())))),
          style = Style(fg = Color.Blue, border = true)
        )
      ),
      input = None
    )

    val frame = AnsiRenderer.buildFrame(curr)
    assert(frame.cells(0)(9).ch == '┐')
    assert(frame.cells(3)(9).ch == '┘')
    assert(frame.cells(1)(9).ch == '│')

    val ansi = AnsiRenderer.renderDiff(Some(AnsiRenderer.buildFrame(prev)), frame)
    assert(!ansi.contains(";11H"))
