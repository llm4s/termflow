package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.io.StringReader
import java.io.StringWriter
import java.nio.file.Path

class AnsiRendererSpec extends AnyFunSuite:
  private def testRenderMetrics(): RenderMetrics =
    new RenderMetrics(
      config = MetricsConfig(enabled = false),
      logger = FrameworkLog(LoggingConfig(LogPath(Path.of("target", "termflow-renderer-test.log"))))
    )

  private def captureRendererOut(renderer: TuiRenderer, root: RootNode, err: Option[TermFlowError] = None): String =
    val out = new StringWriter()
    val backend = new TerminalBackend:
      override def reader        = new StringReader("")
      override def writer        = out
      override def width: Int    = root.width
      override def height: Int   = root.height
      override def close(): Unit = ()
    renderer.render(root, err, backend, testRenderMetrics())
    out.toString

  private def captureAnsiRendererOut(root: RootNode, inputOnly: Boolean = false): String =
    val out = new StringWriter()
    val backend = new TerminalBackend:
      override def reader        = new StringReader("")
      override def writer        = out
      override def width: Int    = root.width
      override def height: Int   = root.height
      override def close(): Unit = ()
    given TerminalBackend = backend
    if inputOnly then AnsiRenderer.renderInputOnly(root)
    else AnsiRenderer.render(root)
    out.toString

  final private class VirtualScreen(width: Int, height: Int):
    private val rows   = Array.fill(height, width)(' ')
    private var cursor = Coord(XCoord(1), YCoord(1))

    def rowText(row: Int): String =
      rows(row - 1).mkString

    def applyAnsi(ansi: String): Unit =
      var i = 0
      while i < ansi.length do
        if ansi.charAt(i) == '\u001b' && i + 1 < ansi.length && ansi.charAt(i + 1) == '[' then
          val commandStart = i + 2
          var j            = commandStart
          while j < ansi.length && !ansi.charAt(j).isLetter do j += 1
          if j < ansi.length then
            val params  = ansi.substring(commandStart, j)
            val command = ansi.charAt(j)
            command match
              case 'H' =>
                val parts = params.split(";", -1)
                val row   = parts.headOption.filter(_.nonEmpty).map(_.toInt).getOrElse(1)
                val col   = parts.lift(1).filter(_.nonEmpty).map(_.toInt).getOrElse(1)
                cursor = Coord(XCoord(col), YCoord(row))
              case 'K' if params == "2" =>
                java.util.Arrays.fill(rows(cursor.y.value - 1), ' ')
                cursor = Coord(XCoord(1), cursor.y)
              case 'm' =>
                ()
              case _ =>
                ()
            i = j + 1
          else i += 1
        else
          val x = cursor.x.value - 1
          val y = cursor.y.value - 1
          if y >= 0 && y < height && x >= 0 && x < width then rows(y)(x) = ansi.charAt(i)
          cursor = Coord(cursor.x + 1, cursor.y)
          i += 1

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

    val out = captureAnsiRendererOut(root)
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

    val out = captureAnsiRendererOut(root, inputOnly = true)
    assert(out.contains(AnsiRenderer.moveTo(XCoord(1), YCoord(5)))) // clear starts at col 1
    assert(out.contains("\u001b[2K"))
    assert(out.contains(" "))                                        // padded trailing area
    assert(out.contains(AnsiRenderer.moveTo(XCoord(13), YCoord(5)))) // x=10 + clamped cursor len(abc)=3

  test("renderInputOnly keeps a fixed prompt prefix visible while horizontally scrolling"):
    val root = RootNode(
      width = 20,
      height = 6,
      children = Nil,
      input = Some(
        InputNode(
          XCoord(2),
          YCoord(5),
          prompt = ">> abcdef",
          style = Style(fg = Color.Green),
          cursor = 9,
          lineWidth = 6,
          prefixLength = 3
        )
      )
    )

    val out = captureAnsiRendererOut(root, inputOnly = true)
    assert(out.contains(">> def"))
    assert(out.contains(AnsiRenderer.moveTo(XCoord(8), YCoord(5))))

  test("renderInputOnly clips the input viewport to the remaining terminal width"):
    val root = RootNode(
      width = 12,
      height = 6,
      children = Nil,
      input = Some(
        InputNode(
          XCoord(10),
          YCoord(5),
          prompt = "abcdef",
          style = Style(fg = Color.Blue),
          cursor = 6,
          lineWidth = 8
        )
      )
    )

    val out = captureAnsiRendererOut(root, inputOnly = true)
    assert(out.contains("def"))
    assert(out.contains(AnsiRenderer.moveTo(XCoord(12), YCoord(5))))

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

  test("buildFrame uses the same bounded prompt viewport as renderInputOnly"):
    val root = RootNode(
      width = 12,
      height = 4,
      children = Nil,
      input = Some(
        InputNode(
          XCoord(2),
          YCoord(3),
          prompt = ">> abcdef",
          style = Style(fg = Color.Green),
          cursor = 9,
          lineWidth = 6,
          prefixLength = 3
        )
      )
    )

    val frame = AnsiRenderer.buildFrame(root)
    assert(frame.cells(2)(1).ch == '>')
    assert(frame.cells(2)(2).ch == '>')
    assert(frame.cells(2)(3).ch == ' ')
    assert(frame.cells(2)(4).ch == 'd')
    assert(frame.cells(2)(5).ch == 'e')
    assert(frame.cells(2)(6).ch == 'f')
    assert(frame.cursor.contains(Coord(XCoord(8), YCoord(3))))

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
    assert(ansi.contains(AnsiRenderer.moveTo(XCoord(1), YCoord(1))))
    assert(ansi.contains("\u001b[2K"))
    assert(ansi.contains("ab"))

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

  test("renderDiff moves the hardware cursor when cursor moves without repainting the row"):
    val prev = RootNode(
      width = 20,
      height = 5,
      children = Nil,
      input = Some(InputNode(XCoord(2), YCoord(4), "[]> new abcdefg", Style(), cursor = 12))
    )
    val curr = RootNode(
      width = 20,
      height = 5,
      children = Nil,
      input = Some(InputNode(XCoord(2), YCoord(4), "[]> new abcdefg", Style(), cursor = 8))
    )

    val ansi = AnsiRenderer.renderDiff(Some(AnsiRenderer.buildFrame(prev)), AnsiRenderer.buildFrame(curr))
    assert(ansi.contains(AnsiRenderer.moveTo(XCoord(10), YCoord(4))))
    assert(!ansi.contains("\u001b[2K"))

  test("moving left inside a scrolled input keeps the trailing suffix visible"):
    val renderer = SimpleANSIRenderer()
    val first = RootNode(
      width = 6,
      height = 3,
      children = Nil,
      input = Some(InputNode(XCoord(1), YCoord(2), "abcdefghi", Style(), cursor = 9, lineWidth = 6))
    )
    val second = RootNode(
      width = 6,
      height = 3,
      children = Nil,
      input = Some(InputNode(XCoord(1), YCoord(2), "abcdefghi", Style(), cursor = 6, lineWidth = 6))
    )

    val screen = new VirtualScreen(width = 6, height = 3)

    screen.applyAnsi(captureRendererOut(renderer, first))
    screen.applyAnsi(captureRendererOut(renderer, second))

    assert(screen.rowText(2).contains("ghi"))

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
    assert(ansi.contains("\u001b[2K"))

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
    assert(ansi.contains(AnsiRenderer.moveTo(XCoord(1), YCoord(4))))
    assert(ansi.contains("\u001b[2K"))
    assert(ansi.contains("[]>"))
    assert(ansi.contains("hi"))

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

  test("diff reports changed cell count and emitted ANSI"):
    val prev = RootNode(
      width = 6,
      height = 2,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("aa", Style())))),
      input = None
    )
    val curr = RootNode(
      width = 6,
      height = 2,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("ab", Style())))),
      input = None
    )

    val result = AnsiRenderer.diff(Some(AnsiRenderer.buildFrame(prev)), AnsiRenderer.buildFrame(curr))
    assert(result.changedCells == 1)
    assert(result.changedRows == 1)
    assert(result.ansi.nonEmpty)

  test("SimpleANSIRenderer clears and repaints after frame resize"):
    val renderer = SimpleANSIRenderer()
    val first = RootNode(
      width = 8,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("first", Style())))),
      input = None
    )
    val second = RootNode(
      width = 18,
      height = 6,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("second", Style())))),
      input = None
    )

    val out = captureRendererOut(renderer, first) + captureRendererOut(renderer, second)
    assert(out.contains(ANSI.clearScreen))
    assert(out.contains(ANSI.homeCursor))

  test("SimpleANSIRenderer does not emit a full-screen clear when only the prompt cursor moves"):
    val renderer = SimpleANSIRenderer()
    val first = RootNode(
      width = 20,
      height = 5,
      children = Nil,
      input = Some(InputNode(XCoord(2), YCoord(4), ">> hello", Style(), cursor = 8, lineWidth = 10, prefixLength = 3))
    )
    val second = RootNode(
      width = 20,
      height = 5,
      children = Nil,
      input = Some(InputNode(XCoord(2), YCoord(4), ">> hello", Style(), cursor = 5, lineWidth = 10, prefixLength = 3))
    )

    captureRendererOut(renderer, first)
    val secondOut = captureRendererOut(renderer, second)
    assert(secondOut.nonEmpty)
    assert(!secondOut.contains(ANSI.clearScreen))
    assert(!secondOut.contains(ANSI.homeCursor))

  test("SimpleANSIRenderer falls back to a full repaint when many rows change"):
    val renderer = SimpleANSIRenderer()
    val first = RootNode(
      width = 20,
      height = 8,
      children = List.tabulate(2)(row => TextNode(XCoord(1), YCoord(row + 1), List(Text(s"row-$row", Style())))),
      input = None
    )
    val second = RootNode(
      width = 20,
      height = 8,
      children = List.tabulate(7)(row => TextNode(XCoord(1), YCoord(row + 1), List(Text(s"changed-$row", Style())))),
      input = None
    )

    captureRendererOut(renderer, first)
    val secondOut = captureRendererOut(renderer, second)
    assert(secondOut.contains(ANSI.clearScreen))
    assert(secondOut.contains(ANSI.homeCursor))
