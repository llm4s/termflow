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
  final case class RenderCell(ch: Char, style: Style)
  final case class RenderFrame(
    width: Int,
    height: Int,
    cells: Array[Array[RenderCell]],
    cursor: Option[Coord]
  )
  final case class DiffResult(ansi: String, changedCells: Int)
  private val blankCell = RenderCell(' ', Style())

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
    val out = new StringBuilder
    root.children.foreach(renderNode(_, out))
    root.input.foreach(renderInput(_, out))
    print(out.toString)

  /** Re-render only the input, leaving existing children intact. */
  def renderInputOnly(root: RootNode): Unit =
    val out = new StringBuilder
    root.input.foreach(renderInput(_, out))
    print(out.toString)

  private def renderNode(v: VNode, out: StringBuilder): Unit = v match
    case TextNode(x, y, l) =>
      out.append(moveTo(x, y))
      l.foreach { case Text(str, style) =>
        out.append(styleToAnsi(style)).append(str).append(reset)
      }

    case BoxNode(x, y, w, h, children, style) =>
      if style.border then drawBorder(x, y, w, h, style.fg, out)
      children.foreach(renderNode(_, out))

    case _: InputNode => () // handled separately

  private def drawBorder(x: XCoord, y: YCoord, w: Int, h: Int, color: Color, out: StringBuilder): Unit =
    val inner      = math.max(0, w - 2)
    val horizontal = "─" * inner
    out.append(moveTo(x, y)).append(colorToAnsi(color, isBg = false)).append(s"┌$horizontal┐")
    (1 until (h - 1)).foreach(row => out.append(moveTo(x, y + row)).append(s"│${" " * inner}│"))
    out.append(moveTo(x, y + (h - 1))).append(s"└$horizontal┘")
    out.append(reset)

  private def renderInput(inp: InputNode, out: StringBuilder): Unit =
    val rendered = inp.prompt

    // Clamp cursor index within the rendered string
    val cursorIndex =
      if inp.cursor >= 0 && inp.cursor <= rendered.length then inp.cursor
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
      if inp.lineWidth > 0 then inp.lineWidth
      else rendered.length

    val printedWidth = rendered.length
    if targetWidth > printedWidth then
      val padding = " " * (targetWidth - printedWidth)
      out.append(baseAnsi).append(padding).append(reset): Unit

    // Place the hardware cursor at the logical editing position.
    out.append(moveTo(inp.x + cursorIndex, inp.y))

  /** Build a full terminal frame from a root node for diff-based rendering. */
  def buildFrame(root: RootNode): RenderFrame =
    def nodeExtents(node: VNode): (Int, Int) = node match
      case TextNode(x, y, segments) =>
        val textWidth = segments.foldLeft(0)((acc, seg) => acc + seg.txt.length)
        val right     = x.value + math.max(0, textWidth - 1)
        (right, y.value)

      case BoxNode(x, y, w, h, children, _) =>
        val boxRight = x.value + math.max(0, w - 1)
        val boxBot   = y.value + math.max(0, h - 1)
        children.foldLeft((boxRight, boxBot)) { case ((mx, my), child) =>
          val (cx, cy) = nodeExtents(child)
          (math.max(mx, cx), math.max(my, cy))
        }

      case InputNode(x, y, prompt, _, _, lineWidth) =>
        val w     = if lineWidth > 0 then lineWidth else prompt.length
        val right = x.value + math.max(0, w - 1)
        (right, y.value)

    val (contentMaxX, contentMaxY) =
      root.children.foldLeft((1, 1)) { case ((mx, my), node) =>
        val (nx, ny) = nodeExtents(node)
        (math.max(mx, nx), math.max(my, ny))
      }

    val (inputMaxX, inputMaxY) =
      root.input match
        case Some(inp) => nodeExtents(inp)
        case None      => (1, 1)

    val width                 = math.max(1, math.max(root.width, math.max(contentMaxX, inputMaxX)))
    val height                = math.max(1, math.max(root.height, math.max(contentMaxY, inputMaxY)))
    val cells                 = Array.fill(height, width)(blankCell)
    var cursor: Option[Coord] = None

    def putCell(x1: Int, y1: Int, cell: RenderCell): Unit =
      val x = x1 - 1
      val y = y1 - 1
      if x >= 0 && x < width && y >= 0 && y < height then cells(y)(x) = cell

    def drawString(x: Int, y: Int, str: String, style: Style): Unit =
      var col = x
      str.foreach { ch =>
        putCell(col, y, RenderCell(ch, style))
        col += 1
      }

    def drawBorder(x: Int, y: Int, w: Int, h: Int, style: Style): Unit =
      if w > 0 && h > 0 then
        var dx = 0
        while dx < w do
          putCell(x + dx, y, RenderCell('─', style))
          if h > 1 then putCell(x + dx, y + h - 1, RenderCell('─', style))
          dx += 1

        var dy = 0
        while dy < h do
          putCell(x, y + dy, RenderCell('│', style))
          if w > 1 then putCell(x + w - 1, y + dy, RenderCell('│', style))
          dy += 1

        putCell(x, y, RenderCell('┌', style))
        if w > 1 then putCell(x + w - 1, y, RenderCell('┐', style))
        if h > 1 then putCell(x, y + h - 1, RenderCell('└', style))
        if w > 1 && h > 1 then putCell(x + w - 1, y + h - 1, RenderCell('┘', style))

    def drawNode(node: VNode): Unit = node match
      case TextNode(x, y, segments) =>
        var col = x.value
        segments.foreach { case Text(str, style) =>
          drawString(col, y.value, str, style)
          col += str.length
        }

      case BoxNode(x, y, w, h, children, style) =>
        if style.border then drawBorder(x.value, y.value, w, h, Style(fg = style.fg))
        children.foreach(drawNode)

      case _: InputNode =>
        ()

    root.children.foreach(drawNode)

    root.input.foreach { inp =>
      val prompt = inp.prompt
      val style  = inp.style
      drawString(inp.x.value, inp.y.value, prompt, style)
      val targetWidth =
        if inp.lineWidth > 0 then inp.lineWidth
        else prompt.length
      if targetWidth > prompt.length then
        drawString(inp.x.value + prompt.length, inp.y.value, " " * (targetWidth - prompt.length), style)

      val clamped =
        if inp.cursor >= 0 && inp.cursor <= prompt.length then inp.cursor
        else prompt.length
      cursor = Some(Coord(inp.x + clamped, inp.y))
    }

    RenderFrame(width, height, cells, cursor)

  /** Diff two frames and emit only changed runs plus cursor movement. */
  def diff(prev: Option[RenderFrame], current: RenderFrame): DiffResult =
    def cellAt(frame: Option[RenderFrame], row: Int, col: Int): RenderCell =
      frame match
        case Some(f) if row < f.height && col < f.width => f.cells(row)(col)
        case _                                          => blankCell

    val out               = new StringBuilder
    val maxH              = math.max(prev.map(_.height).getOrElse(0), current.height)
    val maxW              = math.max(prev.map(_.width).getOrElse(0), current.width)
    var changedCellsCount = 0

    def appendChangedRun(row: Int, start: Int, end: Int): Unit =
      changedCellsCount += (end - start)
      out.append(moveTo(XCoord(start + 1), YCoord(row + 1)))
      var cursor = start
      while cursor < end do
        val style = cellAt(Some(current), row, cursor).style
        out.append(reset).append(styleToAnsi(style))
        var j = cursor
        while j < end && cellAt(Some(current), row, j).style == style do
          out.append(cellAt(Some(current), row, j).ch)
          j += 1
        cursor = j
      out.append(reset)

    var row = 0
    while row < maxH do
      var col = 0
      while col < maxW do
        if cellAt(prev, row, col) == cellAt(Some(current), row, col) then col += 1
        else
          val start = col
          while col < maxW && cellAt(prev, row, col) != cellAt(Some(current), row, col) do col += 1
          appendChangedRun(row, start, col)
      row += 1

    val prevCursor = prev.flatMap(_.cursor)
    // Place the hardware cursor if content changed, or cursor itself moved.
    // This preserves editing position without emitting output on identical frames.
    if changedCellsCount > 0 || prevCursor != current.cursor then current.cursor.foreach(c => out.append(moveTo(c)))

    DiffResult(out.toString, changedCellsCount)

  def renderDiff(prev: Option[RenderFrame], current: RenderFrame): String =
    diff(prev, current).ansi

final case class SimpleANSIRenderer() extends TuiRenderer:
  private var lastFrame: Option[AnsiRenderer.RenderFrame] = None

  override def render(textNode: RootNode, err: Option[TermFlowError]): Unit =
    val currentFrame = AnsiRenderer.buildFrame(textNode)
    val diffResult   = AnsiRenderer.diff(lastFrame, currentFrame)
    val ansi         = diffResult.ansi
    if ansi.nonEmpty then
      print(ansi)
      Console.out.flush()
    if RenderMetrics.isEnabled then
      val bytes = ansi.getBytes("UTF-8").length
      RenderMetrics.recordRender(diffResult.changedCells, bytes)
    lastFrame = Some(currentFrame)

object ACSUtils:
  def EnterAlternateBuffer(): Unit = print("\u001b[?1049h")
  @deprecated("Use EnterAlternateBuffer", "0.1.0")
  def EnterAlternatBuffer(): Unit = EnterAlternateBuffer()
  def EnterNormalBuffer(): Unit   = print("\u001b[?1049l")
  def SaveCursor(): Unit          = print("\u001b7")
  def RestoreCursor(): Unit       = print("\u001b8")
  def ClearCurToEL(): Unit        = print("\u001b0K")
  def ClearCurrentLine(): Unit    = print("\u001b[2K")
  def ClearScreen(): Unit         = print("\u001b[2J")
