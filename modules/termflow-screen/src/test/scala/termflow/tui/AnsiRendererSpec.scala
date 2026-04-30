package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.io.StringReader
import java.io.StringWriter

class AnsiRendererSpec extends AnyFunSuite:

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

  test("moveTo generates correct ANSI escape sequence"):
    assert(AnsiRenderer.moveTo(XCoord(1), YCoord(1)) == "[1;1H")
    assert(AnsiRenderer.moveTo(XCoord(10), YCoord(5)) == "[5;10H")
    assert(AnsiRenderer.moveTo(XCoord(80), YCoord(24)) == "[24;80H")

  test("moveTo with Coord generates correct ANSI escape sequence"):
    assert(AnsiRenderer.moveTo(Coord(XCoord(1), YCoord(1))) == "[1;1H")
    assert(AnsiRenderer.moveTo(Coord(XCoord(10), YCoord(5))) == "[5;10H")

  test("ANSI constants are correctly defined"):
    assert(ANSI.saveCursor == "[s")
    assert(ANSI.restoreCursor == "[u")
    assert(ANSI.enterAltBuffer == "[?1049h")
    assert(ANSI.exitAltBuffer == "[?1049l")
    assert(ANSI.clearScreen == "[2J")
    assert(ANSI.homeCursor == "[H")
    assert(ANSI.hideCursor == "[?25l")
    assert(ANSI.showCursor == "[?25h")
    assert(ANSI.enableBracketedPaste == "[?2004h")
    assert(ANSI.disableBracketedPaste == "[?2004l")
    assert(ANSI.enableMouse.contains("?1006h"))
    assert(ANSI.disableMouse.contains("?1006l"))

  test("clearPatch returns the clear-screen sequence"):
    assert(AnsiRenderer.clearPatch == ANSI.clearScreen)

  test("AnsiRenderer.clear writes the clear-screen sequence to the backend"):
    val out = new StringWriter()
    val backend = new TerminalBackend:
      override def reader        = new StringReader("")
      override def writer        = out
      override def width: Int    = 10
      override def height: Int   = 10
      override def close(): Unit = ()
    given TerminalBackend = backend
    AnsiRenderer.clear()
    assert(out.toString == ANSI.clearScreen)

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
    assert(!out.contains("╭"), "default chars should be sharp, not rounded")

    val rounded = root.copy(children = root.children.collect { case b @ BoxNode(_, _, _, _, _, _, _) =>
      b.copy(chars = BorderChars.rounded)
    })
    val outR = captureAnsiRendererOut(rounded)
    assert(outR.contains("╭"))
    assert(outR.contains("╯"))
    assert(!outR.contains("┌"), "rounded chars should not produce sharp corners")

    val frame  = AnsiRenderer.buildFrame(rounded)
    val topRow = frame.cells(0).map(_.ch).mkString
    val botRow = frame.cells(2).map(_.ch).mkString
    assert(topRow.contains('╭') && topRow.contains('╮'), s"top row should have rounded corners: $topRow")
    assert(botRow.contains('╰') && botRow.contains('╯'), s"bottom row should have rounded corners: $botRow")
    assert(out.contains("[1m"))
    assert(!out.contains(ANSI.hideCursor))
    assert(out.contains("[2K"))
    assert(out.contains(AnsiRenderer.moveTo(XCoord(2), YCoord(4))))

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
    assert(out.contains(AnsiRenderer.moveTo(XCoord(1), YCoord(5))))
    assert(out.contains("[2K"))
    assert(out.contains(" "))
    assert(out.contains(AnsiRenderer.moveTo(XCoord(13), YCoord(5))))

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
    assert(ansi.contains("[2K"))
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
    assert(!ansi.contains("[2K"))

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
    assert(ansi.contains("[2K"))

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
    assert(ansi.contains("[2K"))
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

  // ---- Extended style attributes ----

  private def styleSgr(s: Style, extendedStyles: Boolean = true): String =
    val root = RootNode(
      width = 10,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("x", s)))),
      input = None
    )
    AnsiRenderer.renderPatch(root, ColorDepth.Ansi8, extendedStyles)

  test("italic, dim, reverse, blink, strikethrough emit their SGR codes when supported"):
    val out = styleSgr(
      Style(italic = true, dim = true, reverse = true, blink = true, strikethrough = true)
    )
    assert(out.contains("[2m"), s"dim missing: $out")
    assert(out.contains("[3m"), s"italic missing: $out")
    assert(out.contains("[5m"), s"blink missing: $out")
    assert(out.contains("[7m"), s"reverse missing: $out")
    assert(out.contains("[9m"), s"strikethrough missing: $out")

  test("extended style codes are stripped when capability is disabled, bold/underline still emit"):
    val out =
      styleSgr(Style(bold = true, underline = true, italic = true, reverse = true), extendedStyles = false)
    assert(out.contains("[1m"), s"bold should still emit: $out")
    assert(out.contains("[4m"), s"underline should still emit: $out")
    assert(!out.contains("[3m"), s"italic should be stripped: $out")
    assert(!out.contains("[7m"), s"reverse should be stripped: $out")

  test("Style defaults leave all extended attributes off"):
    val s = Style()
    assert(!s.italic && !s.dim && !s.reverse && !s.strikethrough && !s.blink)
    val out = styleSgr(s)
    Seq("[2m", "[3m", "[5m", "[7m", "[9m").foreach { code =>
      assert(!out.contains(code), s"unexpected code $code in: $out")
    }

  // ---- Unicode display width ----

  test("buildFrame places a wide CJK glyph in one cell + a width=0 continuation"):
    val root = RootNode(
      width = 6,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("中A", Style())))),
      input = None
    )
    val frame = AnsiRenderer.buildFrame(root)
    assert(frame.cells(0)(0).ch == '中')
    assert(frame.cells(0)(0).width == 2)
    assert(frame.cells(0)(1).width == 0, "continuation cell after wide glyph")
    assert(frame.cells(0)(2).ch == 'A')
    assert(frame.cells(0)(2).width == 1)

  test("renderDiff emits the wide glyph once and skips its continuation cell"):
    val root = RootNode(
      width = 4,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("中A", Style())))),
      input = None
    )
    val ansi    = AnsiRenderer.renderDiff(None, AnsiRenderer.buildFrame(root))
    val zhCount = ansi.count(_ == '中')
    val aCount  = ansi.count(_ == 'A')
    assert(zhCount == 1, s"wide glyph should appear exactly once in: $ansi")
    assert(aCount == 1, s"narrow glyph should appear exactly once in: $ansi")

  test("nodeExtents accounts for wide-char display width"):
    val root = RootNode(
      width = 1,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("中日韓", Style())))),
      input = None
    )
    val frame = AnsiRenderer.buildFrame(root)
    assert(frame.width >= 6, s"expected frame width >= 6, got ${frame.width}")

  // ---- renderPatch / depth & extended overloads ----

  test("renderPatch default overload uses Ansi8 depth and extended styles"):
    val root = RootNode(
      width = 6,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("hi", Style(fg = Color.Red))))),
      input = None
    )
    val out = AnsiRenderer.renderPatch(root)
    assert(out.contains("[31m"), s"red SGR missing: $out")
    assert(out.contains("hi"))

  test("renderPatch (depth-only overload) honours the supplied depth"):
    val root = RootNode(
      width = 6,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("hi", Style(fg = Color.Rgb(10, 20, 30)))))),
      input = None
    )
    val out = AnsiRenderer.renderPatch(root, ColorDepth.Truecolor)
    assert(out.contains("[38;2;10;20;30m"), s"truecolor SGR missing: $out")

  test("inputPatch returns just the input portion"):
    val root = RootNode(
      width = 20,
      height = 5,
      children = Nil,
      input = Some(InputNode(XCoord(1), YCoord(2), "abc", Style(), cursor = 1))
    )
    val patch = AnsiRenderer.inputPatch(root)
    assert(patch.contains("abc"))
    // The patch must place the cursor; the AnsiRenderer.moveTo(...) form
    // appears at least once.
    assert(patch.contains("[2K"))

  test("inputPatch with (depth, extendedStyles) overload honours both"):
    val root = RootNode(
      width = 20,
      height = 5,
      children = Nil,
      input = Some(
        InputNode(
          XCoord(1),
          YCoord(2),
          "abc",
          Style(fg = Color.Rgb(1, 2, 3), italic = true),
          cursor = 1
        )
      )
    )
    val truecolor  = AnsiRenderer.inputPatch(root, ColorDepth.Truecolor, extendedStyles = true)
    val noExtended = AnsiRenderer.inputPatch(root, ColorDepth.Truecolor, extendedStyles = false)
    assert(truecolor.contains("[3m"), s"italic SGR missing: $truecolor")
    assert(!noExtended.contains("[3m"), s"italic SGR should be stripped: $noExtended")
    assert(truecolor.contains("[38;2;1;2;3m"))

  test("inputPatch returns empty when there is no input and no overlay"):
    val root = RootNode(width = 10, height = 1, children = Nil, input = None)
    assert(AnsiRenderer.inputPatch(root).isEmpty)

  // ---- Overlays ----

  test("renderPatch wipes overlay rectangle and draws overlay children"):
    val overlay = Overlay(
      width = 6,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("over", Style())))),
      position = OverlayPosition.TopLeft
    )
    val root = RootNode(
      width = 20,
      height = 6,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("background", Style())))),
      input = None,
      overlays = List(overlay)
    )
    val out = AnsiRenderer.renderPatch(root)
    assert(out.contains("over"), s"overlay text missing: $out")
    // The overlay rectangle is wiped with spaces; the wipe write begins
    // at the overlay's resolved coordinates.
    assert(out.contains(AnsiRenderer.moveTo(XCoord(1), YCoord(1))))

  test("modal overlay suppresses base-view input but renders overlay input"):
    val overlay = Overlay(
      width = 10,
      height = 3,
      children = Nil,
      input = Some(InputNode(XCoord(2), YCoord(2), "modal", Style(), cursor = 5)),
      position = OverlayPosition.TopLeft,
      inputCapture = InputCapture.Modal
    )
    val root = RootNode(
      width = 20,
      height = 6,
      children = Nil,
      input = Some(InputNode(XCoord(1), YCoord(5), "base", Style(), cursor = 4)),
      overlays = List(overlay)
    )
    val out = AnsiRenderer.renderPatch(root)
    assert(out.contains("modal"))
    assert(!out.contains("base"))

  test("inputPatch with a modal overlay routes to the overlay's input"):
    val overlay = Overlay(
      width = 10,
      height = 3,
      children = Nil,
      input = Some(InputNode(XCoord(2), YCoord(2), "modal", Style(), cursor = 5)),
      position = OverlayPosition.TopLeft,
      inputCapture = InputCapture.Modal
    )
    val root = RootNode(
      width = 20,
      height = 6,
      children = Nil,
      input = Some(InputNode(XCoord(1), YCoord(5), "base", Style(), cursor = 4)),
      overlays = List(overlay)
    )
    val patch = AnsiRenderer.inputPatch(root)
    assert(patch.contains("modal"))
    assert(!patch.contains("base"))

  // ---- Layout in RootNode ----

  test("renderPatch resolves a RootNode-level Layout into rendered nodes"):
    import termflow.tui.ScreenPrelude.*
    val layout = Layout.Column(
      gap = 0,
      children = List(
        Layout.Elem(TextNode(1.x, 1.y, List(Text("alpha", Style())))),
        Layout.Elem(TextNode(1.x, 1.y, List(Text("beta", Style()))))
      )
    )
    val root = RootNode(
      width = 20,
      height = 4,
      children = Nil,
      input = None,
      layout = Some(layout)
    )
    val out = AnsiRenderer.renderPatch(root)
    assert(out.contains("alpha"))
    assert(out.contains("beta"))

  test("buildFrame resolves a RootNode-level Layout into the cell grid"):
    import termflow.tui.ScreenPrelude.*
    val layout = Layout.Column(
      gap = 0,
      children = List(
        Layout.Elem(TextNode(1.x, 1.y, List(Text("alpha", Style())))),
        Layout.Elem(TextNode(1.x, 1.y, List(Text("beta", Style()))))
      )
    )
    val root = RootNode(
      width = 20,
      height = 4,
      children = Nil,
      input = None,
      layout = Some(layout)
    )
    val frame = AnsiRenderer.buildFrame(root)
    val row0  = frame.cells(0).map(_.ch).mkString.trim
    val row1  = frame.cells(1).map(_.ch).mkString.trim
    assert(row0.startsWith("alpha"))
    assert(row1.startsWith("beta"))
