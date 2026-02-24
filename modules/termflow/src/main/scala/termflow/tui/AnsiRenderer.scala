package termflow.tui

/** X coordinate wrapper (1-based column). */
opaque type XCoord = Int
object XCoord:
  def apply(value: Int): XCoord = value

  extension (x: XCoord)
    def value: Int         = x
    def +(dx: Int): XCoord = x + dx
    def -(dx: Int): XCoord = x - dx

/** Y coordinate wrapper (1-based row). */
opaque type YCoord = Int
object YCoord:
  def apply(value: Int): YCoord = value

  extension (y: YCoord)
    def value: Int         = y
    def +(dy: Int): YCoord = y + dy
    def -(dy: Int): YCoord = y - dy

final case class Coord(x: XCoord, y: YCoord)

object ANSI:
  val saveCursor     = "\u001b[s"
  val restoreCursor  = "\u001b[u"
  val enterAltBuffer = "\u001b[?1049h"
  val exitAltBuffer  = "\u001b[?1049l"
  val clearScreen    = "\u001b[2J"
  val homeCursor     = "\u001b[H"
  val hideCursor     = "\u001b[?25l"
  val showCursor     = "\u001b[?25h"

object AnsiRenderer:
  private val reset = "\u001b[0m"

  def moveTo(x: XCoord, y: YCoord): String =
    s"\u001b[${y.value};${x.value}H"

  def moveTo(c: Coord): String =
    s"\u001b[${c.y.value};${c.x.value}H"

  private def colorToAnsi(c: Color, isBg: Boolean): String = c match
    case Color.Default => ""
    case _ =>
      val base = if isBg then 40 else 30
      s"\u001b[${base + (c.ordinal - 1)}m"

  private def styleToAnsi(s: Style): String =
    val b = new StringBuilder
    if s.bold then b.append("\u001b[1m")
    if s.underline then b.append("\u001b[4m")
    b.append(colorToAnsi(s.fg, isBg = false))
    b.append(colorToAnsi(s.bg, isBg = true))
    b.toString

  def clear(): Unit =
    print(ANSI.clearScreen)

  def render(root: RootNode): Unit =
    root.children.foreach(renderNode)
    root.input.foreach(renderInput)

  /** Re-render only the input, leaving existing children intact. */
  def renderInputOnly(root: RootNode): Unit =
    root.input.foreach(renderInput)

  private def renderNode(v: VNode): Unit = v match
    case TextNode(x, y, l) =>
      print(moveTo(x, y))
      l.foreach { case Text(str, style) =>
        print(styleToAnsi(style) + str + reset)
      }

    case BoxNode(x, y, w, h, children, style) =>
      if style.border then drawBorder(x, y, w, h, style.fg)
      children.foreach(renderNode)

    case _: InputNode => () // handled separately

  private def drawBorder(x: XCoord, y: YCoord, w: Int, h: Int, color: Color): Unit =
    val inner      = math.max(0, w - 2)
    val horizontal = "─" * inner
    print(moveTo(x, y) + colorToAnsi(color, isBg = false) + s"┌$horizontal┐")
    (1 until (h - 1)).foreach(row => print(moveTo(x, y + row) + s"│${" " * inner}│"))
    print(moveTo(x, y + (h - 1)) + s"└$horizontal┘")
    print(reset)

  private def renderInput(inp: InputNode): Unit =
    val rendered = inp.prompt

    // Clamp cursor index within the rendered string
    val cursorIndex =
      if inp.cursor >= 0 && inp.cursor <= rendered.length then inp.cursor
      else rendered.length

    // Split text around the cursor position
    val (left, right) = rendered.splitAt(cursorIndex)

    // Move to input position and clear the whole line to avoid ghost characters
    print(moveTo(inp.x, inp.y))
    print("\u001b[2K")

    // Hide the hardware cursor and draw an inline visual cursor
    print(ANSI.hideCursor)

    val baseStyle = inp.style
    val baseAnsi  = styleToAnsi(baseStyle)
    // Caret uses reverse video on top of the base style for visibility
    val caretAnsi = baseAnsi + "\u001b[7m"

    // Print left part with base style
    print(baseAnsi + left)

    // Print the character at the cursor position with caret style
    if right.nonEmpty then
      val ch   = right.head
      val tail = right.tail
      print(caretAnsi + ch + reset)
      // Restore base style for the rest
      print(baseAnsi + tail + reset)
    else
      // Cursor at end: show a reversed space
      print(caretAnsi + " " + reset)
      print(baseAnsi + reset)

    // Optionally pad the rest of the line so the background spans a fixed width.
    val targetWidth =
      if inp.lineWidth > 0 then inp.lineWidth
      else rendered.length + 1

    val printedWidth = math.max(rendered.length, cursorIndex + 1)
    if targetWidth > printedWidth then
      val padding = " " * (targetWidth - printedWidth)
      print(baseAnsi + padding + reset)

final case class SimpleANSIRenderer() extends TuiRenderer:
  // Keep track of the last rendered root to avoid unnecessary redraws.
  private var lastRoot: Option[RootNode] = None

  override def render(textNode: RootNode, err: Option[TermFlowError]): Unit =
    lastRoot match
      // If the static children are unchanged, only re-render the input
      case Some(prev) if prev.copy(input = None) == textNode.copy(input = None) =>
        AnsiRenderer.renderInputOnly(textNode)
      case _ =>
        AnsiRenderer.render(textNode)
    lastRoot = Some(textNode)

object ACSUtils:
  def EnterAlternatBuffer(): Unit = print("\u001b[?1049h")
  def EnterNormalBuffer(): Unit   = print("\u001b[?1049l")
  def SaveCursor(): Unit          = print("\u001b7")
  def RestoreCursor(): Unit       = print("\u001b8")
  def ClearCurToEL(): Unit        = print("\u001b0K")
  def ClearCurrentLine(): Unit    = print("\u001b[2K")
  def ClearScreen(): Unit         = print("\u001b[2J")
