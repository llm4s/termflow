package termflow.tui

/**
 * 1-based column coordinate.
 *
 * Opaque over `Int` to prevent accidental mixing with row coordinates. The
 * `TuiPrelude` package provides `2.x` syntax sugar for constructing these
 * at call sites.
 */
opaque type XCoord = Int
object XCoord:

  /** Construct an `XCoord` from a raw 1-based column. */
  def apply(value: Int): XCoord = value

  extension (x: XCoord)

    /** Unwrap to the raw 1-based column. */
    def value: Int = x

    /** Shift right by `dx` columns. */
    def +(dx: Int): XCoord = x + dx

    /** Shift left by `dx` columns. */
    def -(dx: Int): XCoord = x - dx

/**
 * 1-based row coordinate.
 *
 * Opaque over `Int`; see [[XCoord]] for rationale. Construct at call sites
 * with `3.y` via the `TuiPrelude` extension.
 */
opaque type YCoord = Int
object YCoord:

  /** Construct a `YCoord` from a raw 1-based row. */
  def apply(value: Int): YCoord = value

  extension (y: YCoord)

    /** Unwrap to the raw 1-based row. */
    def value: Int = y

    /** Shift down by `dy` rows. */
    def +(dy: Int): YCoord = y + dy

    /** Shift up by `dy` rows. */
    def -(dy: Int): YCoord = y - dy

/** An `(x, y)` cell position on the terminal, 1-based. */
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
  final case class DiffResult(ansi: String, changedCells: Int, changedRows: Int)
  final private case class VisibleInput(text: String, cursorIndex: Int, width: Int)
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

  def clearPatch: String =
    ANSI.clearScreen

  def clear()(using terminal: TerminalBackend): Unit =
    terminal.write(clearPatch)

  def renderPatch(root: RootNode): String =
    val out = new StringBuilder
    root.children.foreach(renderNode(_, out))
    root.input.foreach(renderInput(_, root.width, out))
    out.toString

  def render(root: RootNode)(using terminal: TerminalBackend): Unit =
    terminal.write(renderPatch(root))

  /** Re-render only the input, leaving existing children intact. */
  def renderInputOnly(root: RootNode)(using terminal: TerminalBackend): Unit =
    terminal.write(inputPatch(root))

  /** Build ANSI patch for input-only repaint. */
  def inputPatch(root: RootNode): String =
    val out = new StringBuilder
    root.input.foreach(renderInput(_, root.width, out))
    out.toString

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

  private def visibleInput(inp: InputNode, rootWidth: Int): VisibleInput =
    val clampedCursor =
      if inp.cursor >= 0 && inp.cursor <= inp.prompt.length then inp.cursor
      else inp.prompt.length

    val requestedWidth =
      if inp.lineWidth > 0 then inp.lineWidth
      else math.max(1, inp.prompt.length + 1)

    val remainingWidth = math.max(1, rootWidth - inp.x.value + 1)
    val width          = math.max(1, math.min(requestedWidth, remainingWidth))
    val prefixLength   = math.max(0, math.min(inp.prefixLength, inp.prompt.length))
    val fixedPrefix    = inp.prompt.take(prefixLength).take(width)
    val suffix         = inp.prompt.drop(prefixLength)
    val availableWidth = math.max(0, width - fixedPrefix.length)
    val suffixCursor   = math.max(0, clampedCursor - prefixLength)
    val suffixStart =
      if availableWidth == 0 then 0
      else
        val maxStart              = math.max(0, suffix.length - availableWidth)
        val preferredRightContext = math.min(2, math.max(0, availableWidth - 1))
        val desiredStart          = suffixCursor - availableWidth + preferredRightContext + 1
        math.max(0, math.min(maxStart, desiredStart))
    val visibleSuffix =
      if availableWidth == 0 then ""
      else suffix.slice(suffixStart, suffixStart + availableWidth)
    val visibleText =
      val text = fixedPrefix + visibleSuffix
      if text.length >= width then text.take(width)
      else text + (" " * (width - text.length))
    val unclampedCursorIndex =
      if clampedCursor <= prefixLength then clampedCursor
      else fixedPrefix.length + (suffixCursor - suffixStart)
    val cursorLimit =
      if inp.x.value + width <= rootWidth then width
      else width - 1
    val cursorIndex = math.max(0, math.min(cursorLimit, unclampedCursorIndex))
    VisibleInput(visibleText, cursorIndex, width)

  private def renderInput(inp: InputNode, rootWidth: Int, out: StringBuilder): Unit =
    val visible = visibleInput(inp, rootWidth)

    // Clear full terminal row from column 1, then position to input x.
    // Some terminal emulators behave inconsistently when 2K is emitted away from column 1.
    out.append(moveTo(XCoord(1), inp.y))
    out.append("\u001b[2K")
    out.append(moveTo(inp.x, inp.y))

    val baseStyle = inp.style
    val baseAnsi  = styleToAnsi(baseStyle)
    // Draw the bounded single-line viewport only; never rely on terminal soft-wrap.
    out.append(baseAnsi).append(visible.text).append(reset)

    // Place the hardware cursor at the logical editing position.
    out.append(moveTo(inp.x + visible.cursorIndex, inp.y))

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

      case InputNode(x, y, prompt, _, _, lineWidth, _) =>
        val w     = if lineWidth > 0 then lineWidth else prompt.length
        val right = x.value + math.max(0, w - 1)
        (right, y.value)

    val (contentMaxX, contentMaxY) =
      root.children.foldLeft((1, 1)) { case ((mx, my), node) =>
        val (nx, ny) = nodeExtents(node)
        (math.max(mx, nx), math.max(my, ny))
      }

    val visibleInputOpt = root.input.map(inp => visibleInput(inp, root.width))

    val (inputMaxX, inputMaxY) =
      root.input match
        case Some(inp) =>
          val visible = visibleInputOpt.get
          (inp.x.value + math.max(0, visible.width - 1), inp.y.value)
        case None => (1, 1)

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
      val visible = visibleInput(inp, root.width)
      drawString(inp.x.value, inp.y.value, visible.text, inp.style)
      cursor = Some(Coord(inp.x + visible.cursorIndex, inp.y))
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
    var changedRowsCount  = 0

    def appendRepaintedRow(row: Int): Unit =
      out.append(moveTo(XCoord(1), YCoord(row + 1)))
      out.append("\u001b[2K")
      var col = 0
      while col < current.width do
        val cell = cellAt(Some(current), row, col)
        if cell == blankCell then col += 1
        else
          out.append(moveTo(XCoord(col + 1), YCoord(row + 1)))
          var cursor = col
          while cursor < current.width && cellAt(Some(current), row, cursor) != blankCell do
            val style = cellAt(Some(current), row, cursor).style
            out.append(reset).append(styleToAnsi(style))
            var j = cursor
            while j < current.width &&
              cellAt(Some(current), row, j) != blankCell &&
              cellAt(Some(current), row, j).style == style
            do
              out.append(cellAt(Some(current), row, j).ch)
              j += 1
            cursor = j
          col = cursor
      out.append(reset)

    var row = 0
    while row < maxH do
      var rowChanged = false
      var col        = 0
      while col < maxW do
        if cellAt(prev, row, col) != cellAt(Some(current), row, col) then
          rowChanged = true
          changedCellsCount += 1
        col += 1
      if rowChanged then
        changedRowsCount += 1
        appendRepaintedRow(row)
      row += 1

    val prevCursor = prev.flatMap(_.cursor)
    // Place the hardware cursor if content changed, or cursor itself moved.
    // This preserves editing position without emitting output on identical frames.
    if changedCellsCount > 0 || prevCursor != current.cursor then current.cursor.foreach(c => out.append(moveTo(c)))

    DiffResult(out.toString, changedCellsCount, changedRowsCount)

  def renderDiff(prev: Option[RenderFrame], current: RenderFrame): String =
    diff(prev, current).ansi

final case class SimpleANSIRenderer() extends TuiRenderer:
  private var lastFrame: Option[AnsiRenderer.RenderFrame] = None
  private val FullRepaintRowThreshold                     = 6

  override def render(
    textNode: RootNode,
    err: Option[TermFlowError],
    terminal: TerminalBackend,
    renderMetrics: RenderMetrics
  ): Unit =
    val currentFrame = AnsiRenderer.buildFrame(textNode)
    val resized      = lastFrame.exists(prev => prev.width != currentFrame.width || prev.height != currentFrame.height)
    val initialDiff =
      if resized then AnsiRenderer.diff(None, currentFrame)
      else AnsiRenderer.diff(lastFrame, currentFrame)
    val shouldFullRepaint =
      resized || initialDiff.changedRows >= math.min(currentFrame.height, FullRepaintRowThreshold)
    val diffResult =
      if shouldFullRepaint then AnsiRenderer.diff(None, currentFrame)
      else initialDiff
    val ansi =
      if shouldFullRepaint then ANSI.clearScreen + ANSI.homeCursor + diffResult.ansi
      else diffResult.ansi
    if ansi.nonEmpty then
      terminal.write(ansi)
      terminal.flush()
    if renderMetrics.isEnabled then
      val bytes = ansi.getBytes("UTF-8").length
      renderMetrics.recordRender(diffResult.changedCells, bytes)
    lastFrame = Some(currentFrame)
