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
    val out = new StringBuilder
    root.children.foreach(renderNode(_, out))
    root.input.foreach(renderInput(_, out))
    print(out.toString)
  }

  /** Re-render only the input, leaving existing children intact. */
  def renderInputOnly(root: RootNode): Unit = {
    val out = new StringBuilder
    root.input.foreach(renderInput(_, out))
    print(out.toString)
  }

  private def renderNode(v: VNode, out: StringBuilder): Unit = v match {
    case TextNode(x, y, l) =>
      out.append(moveTo(x, y))
      l.foreach { case Text(str, style) =>
        out.append(styleToAnsi(style)).append(str).append(reset)
      }

    case BoxNode(x, y, w, h, children, style) =>
      if (style.border) drawBorder(x, y, w, h, style.fg, out)
      children.foreach(renderNode(_, out))

    case _: InputNode => () // handled separately
  }

  private def drawBorder(x: XCoord, y: YCoord, w: Int, h: Int, color: Color, out: StringBuilder): Unit = {
    val inner      = math.max(0, w - 2)
    val horizontal = "─" * inner
    out.append(moveTo(x, y)).append(colorToAnsi(color, isBg = false)).append(s"┌$horizontal┐")
    (1 until (h - 1)).foreach(row => out.append(moveTo(x, y + row)).append(s"│${" " * inner}│"))
    out.append(moveTo(x, y + (h - 1))).append(s"└$horizontal┘")
    out.append(reset)
  }

  private def renderInput(inp: InputNode, out: StringBuilder): Unit = {
    val rendered = inp.prompt

    // Clamp cursor index within the rendered string
    val cursorIndex =
      if (inp.cursor >= 0 && inp.cursor <= rendered.length) inp.cursor
      else rendered.length

    // Move to input position and clear the whole line to avoid ghost characters
    out.append(moveTo(inp.x, inp.y))
    out.append("\u001b[2K")

    val baseStyle = inp.style
    val baseAnsi  = styleToAnsi(baseStyle)
    // Draw full prompt text and use hardware cursor for caret.
    out.append(baseAnsi).append(rendered).append(reset)

    // Optionally pad the rest of the line so the background spans a fixed width.
    val targetWidth =
      if (inp.lineWidth > 0) inp.lineWidth
      else rendered.length

    val printedWidth = rendered.length
    if (targetWidth > printedWidth) {
      val padding = " " * (targetWidth - printedWidth)
      out.append(baseAnsi).append(padding).append(reset): Unit
    }

    // Place the hardware cursor at the logical editing position.
    out.append(moveTo(inp.x + cursorIndex, inp.y))
  }
}

final case class SimpleANSIRenderer() extends TuiRenderer {
  // Keep track of the last rendered root to avoid unnecessary redraws.
  private var lastRoot: Option[RootNode] = None
  private var lastInputPos: Option[Coord] = None

  override def render(textNode: RootNode, err: Option[TermFlowError]): Unit = {
    val currentInputPos = textNode.input.map(inp => Coord(inp.x, inp.y))

    // If input moved (or disappeared), clear the previously used input line
    // to avoid stale duplicated prompts.
    (lastInputPos, currentInputPos) match {
      case (Some(prev), Some(curr)) if prev != curr =>
        print(AnsiRenderer.moveTo(prev))
        print("\u001b[2K")
      case (Some(prev), None) =>
        print(AnsiRenderer.moveTo(prev))
        print("\u001b[2K")
      case _ =>
        ()
    }

    lastRoot match {
      // If the static children are unchanged, only re-render the input
      case Some(prev) if prev.copy(input = None) == textNode.copy(input = None) =>
        AnsiRenderer.renderInputOnly(textNode)
      case _ =>
        AnsiRenderer.render(textNode)
    }

    lastInputPos = currentInputPos
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
