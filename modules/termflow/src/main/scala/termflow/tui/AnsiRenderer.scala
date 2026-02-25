package termflow.tui

/** X coordinate wrapper (1-based column). */
final case class XCoord(value: Int) extends AnyVal {
  def +(dx: Int): XCoord = XCoord(value + dx)
  def -(dx: Int): XCoord = XCoord(value - dx)
}

/** Y coordinate wrapper (1-based row). */
final case class YCoord(value: Int) extends AnyVal {
  def +(dy: Int): YCoord = YCoord(value + dy)
  def -(dy: Int): YCoord = YCoord(value - dy)
}

final case class Coord(x: XCoord, y: YCoord)

object ANSI {
  val saveCursor     = "\u001b[s"
  val restoreCursor  = "\u001b[u"
  val enterAltBuffer = "\u001b[?1049h"
  val exitAltBuffer  = "\u001b[?1049l"
  val clearScreen    = "\u001b[2J"
  val homeCursor     = "\u001b[H"
  val hideCursor     = "\u001b[?25l"
  val showCursor     = "\u001b[?25h"
}

object AnsiRenderer {
  private val reset = "\u001b[0m"

  def moveTo(x: XCoord, y: YCoord): String =
    s"\u001b[${y.value};${x.value}H"

  def moveTo(c: Coord): String =
    s"\u001b[${c.y.value};${c.x.value}H"

  private def colorToAnsi(c: Color, isBg: Boolean): String = c match {
    case Color.Default => ""
    case _             =>
      val base = if (isBg) 40 else 30
      s"\u001b[${base + (c.ordinal - 1)}m"
  }

  private def styleToAnsi(s: Style): String = {
    val b = new StringBuilder
    if (s.bold) b.append("\u001b[1m")
    if (s.underline) b.append("\u001b[4m")
    b.append(colorToAnsi(s.fg, isBg = false))
    b.append(colorToAnsi(s.bg, isBg = true))
    b.toString
  }

  def clear(): Unit =
    print(ANSI.clearScreen)

  def render(root: RootNode): Unit = {
    root.children.foreach(renderNode)
    root.input.foreach(renderInput)
  }

  /** Re-render only the input, leaving existing children intact. */
  def renderInputOnly(root: RootNode): Unit =
    root.input.foreach(renderInput)

  private def renderNode(v: VNode): Unit = v match {
    case TextNode(x, y, l) =>
      print(moveTo(x, y))
      l.foreach { case Text(str, style) =>
        print(styleToAnsi(style) + str + reset)
      }

    case BoxNode(x, y, w, h, children, style) =>
      if (style.border) drawBorder(x, y, w, h, style.fg)
      children.foreach(renderNode)

    case _: InputNode => () // handled separately
  }

  private def drawBorder(x: XCoord, y: YCoord, w: Int, h: Int, color: Color): Unit = {
    val inner      = math.max(0, w - 2)
    val horizontal = "─" * inner
    print(moveTo(x, y) + colorToAnsi(color, isBg = false) + s"┌$horizontal┐")
    (1 until (h - 1)).foreach(row => print(moveTo(x, y + row) + s"│${" " * inner}│"))
    print(moveTo(x, y + (h - 1)) + s"└$horizontal┘")
    print(reset)
  }

  private def renderInput(inp: InputNode): Unit = {
    val rendered = inp.prompt

    // Clamp cursor index within the rendered string
    val cursorIndex =
      if (inp.cursor >= 0 && inp.cursor <= rendered.length) inp.cursor
      else rendered.length

    // Move to input position and clear the whole line to avoid ghost characters
    print(moveTo(inp.x, inp.y))
    print("\u001b[2K")

    val baseStyle = inp.style
    val baseAnsi  = styleToAnsi(baseStyle)
    // Draw full prompt text and use hardware cursor for caret.
    print(baseAnsi + rendered + reset)

    // Optionally pad the rest of the line so the background spans a fixed width.
    val targetWidth =
      if (inp.lineWidth > 0) inp.lineWidth
      else rendered.length

    val printedWidth = rendered.length
    if (targetWidth > printedWidth) {
      val padding = " " * (targetWidth - printedWidth)
      print(baseAnsi + padding + reset)
    }

    // Place the hardware cursor at the logical editing position.
    print(moveTo(inp.x + cursorIndex, inp.y))
  }
}

final case class SimpleANSIRenderer() extends TuiRenderer {
  // Keep track of the last rendered root to avoid unnecessary redraws.
  private var lastRoot: Option[RootNode] = None

  override def render(textNode: RootNode, err: Option[TermFlowError]): Unit = {
    lastRoot match {
      // If the static children are unchanged, only re-render the input
      case Some(prev) if prev.copy(input = None) == textNode.copy(input = None) =>
        AnsiRenderer.renderInputOnly(textNode)
      case _ =>
        AnsiRenderer.render(textNode)
    }
    lastRoot = Some(textNode)
  }
}

object ACSUtils {
  def EnterAlternateBuffer(): Unit = print("\u001b[?1049h")
  @deprecated("Use EnterAlternateBuffer", "0.1.0")
  def EnterAlternatBuffer(): Unit = EnterAlternateBuffer()
  def EnterNormalBuffer(): Unit   = print("\u001b[?1049l")
  def SaveCursor(): Unit          = print("\u001b7")
  def RestoreCursor(): Unit       = print("\u001b8")
  def ClearCurToEL(): Unit        = print("\u001b0K")
  def ClearCurrentLine(): Unit    = print("\u001b[2K")
  def ClearScreen(): Unit         = print("\u001b[2J")
}
