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

  /**
   * xterm bracketed-paste mode on. Pasted text arrives as
   *  `ESC [ 200 ~ ... ESC [ 201 ~`, decoded into a single
   *  [[KeyDecoder.InputKey.Paste]].
   */
  val enableBracketedPaste  = "\u001b[?2004h"
  val disableBracketedPaste = "\u001b[?2004l"

  /**
   * Enable button-event mouse tracking (`?1002`) plus SGR coordinate
   * encoding (`?1006`). With this combo the terminal sends
   * `CSI < button ; col ; row M/m` for every press, release, drag, and
   * scroll-wheel event, decoded into [[KeyDecoder.InputKey.Mouse]].
   *
   * `?1003` (any-event tracking — bare `Move` events) is intentionally
   * left off. It floods the input stream with motion bytes that are
   * rarely useful for TUIs and that cost real CPU on the decode path.
   */
  val enableMouse  = "\u001b[?1006h\u001b[?1002h"
  val disableMouse = "\u001b[?1002l\u001b[?1006l"

object AnsiRenderer:
  private val reset = "\u001b[0m"

  /**
   * One cell on the rendered terminal grid.
   *
   * @param ch    The glyph to emit. For surrogate pairs / multi-cell
   *              graphemes, only the leading code unit is stored here;
   *              richer grapheme support is a future stage.
   * @param style Foreground / background / SGR attributes.
   * @param width Display width in cells: `1` for the typical narrow cell,
   *              `2` for an East Asian Wide / Fullwidth glyph (the next
   *              cell is a continuation), or `0` for a continuation cell
   *              that exists only to absorb the second column of a wide
   *              glyph emitted in the previous cell. The diff loop honours
   *              `width = 0` cells by *not* emitting their `ch`, since the
   *              terminal already advanced past that column when the wide
   *              char was written.
   */
  final case class RenderCell(ch: Char, style: Style, width: Int = 1)
  final case class RenderFrame(
    width: Int,
    height: Int,
    cells: Array[Array[RenderCell]],
    cursor: Option[Coord]
  )
  final case class DiffResult(ansi: String, changedCells: Int, changedRows: Int)
  final private case class VisibleInput(text: String, cursorIndex: Int, width: Int)
  private val blankCell = RenderCell(' ', Style(), 1)

  def moveTo(x: XCoord, y: YCoord): String =
    s"\u001b[${y.value};${x.value}H"

  def moveTo(c: Coord): String =
    s"\u001b[${c.y.value};${c.x.value}H"

  /**
   * Emit an SGR sequence for `c` at the given color depth, downgrading to
   * the closest representable color when the depth is lower than what `c`
   * carries. Returns `""` for `Color.Default` and for `ColorDepth.Mono`.
   *
   * Public-API contract: same return shape as the previous internal helper
   * for the basic 8 colors at any depth >= Ansi8 - existing goldens and
   * snapshot tests stay valid.
   */
  private[tui] def colorToAnsi(c: Color, isBg: Boolean, depth: ColorDepth): String =
    if depth == ColorDepth.Mono then ""
    else
      c match
        case Color.Default => ""
        case named if Color.namedRgb.contains(named) =>
          encodeNamed(named, isBg, depth)
        case Color.Indexed(n) =>
          val nn = math.max(0, math.min(255, n))
          depth match
            case ColorDepth.Indexed256 | ColorDepth.Truecolor =>
              val prefix = if isBg then 48 else 38
              s"\u001b[$prefix;5;${nn}m"
            case ColorDepth.Ansi16 | ColorDepth.Ansi8 =>
              val (r, g, b) = Color.indexedToRgb(nn)
              encodeNamed(downgradeRgb(r, g, b, depth), isBg, depth)
            case ColorDepth.Mono => ""
        case Color.Rgb(r, g, b) =>
          val rr = math.max(0, math.min(255, r))
          val gg = math.max(0, math.min(255, g))
          val bb = math.max(0, math.min(255, b))
          depth match
            case ColorDepth.Truecolor =>
              val prefix = if isBg then 48 else 38
              s"\u001b[$prefix;2;$rr;$gg;${bb}m"
            case ColorDepth.Indexed256 =>
              val n      = Color.nearestIndexed(rr, gg, bb)
              val prefix = if isBg then 48 else 38
              s"\u001b[$prefix;5;${n}m"
            case ColorDepth.Ansi16 | ColorDepth.Ansi8 =>
              encodeNamed(downgradeRgb(rr, gg, bb, depth), isBg, depth)
            case ColorDepth.Mono => ""
        case _ => ""

  private def downgradeRgb(r: Int, g: Int, b: Int, depth: ColorDepth): Color =
    if depth == ColorDepth.Ansi16 then Color.nearestAnsi16(r, g, b)
    else Color.nearestAnsi8(r, g, b)

  private def encodeNamed(c: Color, isBg: Boolean, depth: ColorDepth): String =
    val baseFg = if isBg then 40 else 30
    c match
      case Color.Black   => s"\u001b[${baseFg + 0}m"
      case Color.Red     => s"\u001b[${baseFg + 1}m"
      case Color.Green   => s"\u001b[${baseFg + 2}m"
      case Color.Yellow  => s"\u001b[${baseFg + 3}m"
      case Color.Blue    => s"\u001b[${baseFg + 4}m"
      case Color.Magenta => s"\u001b[${baseFg + 5}m"
      case Color.Cyan    => s"\u001b[${baseFg + 6}m"
      case Color.White   => s"\u001b[${baseFg + 7}m"
      case bright =>
        if depth == ColorDepth.Ansi8 then encodeNamed(brightToBase(bright), isBg, depth)
        else
          val brightBase = if isBg then 100 else 90
          s"\u001b[${brightBase + brightOffset(bright)}m"

  private def brightToBase(c: Color): Color = c match
    case Color.BrightBlack   => Color.Black
    case Color.BrightRed     => Color.Red
    case Color.BrightGreen   => Color.Green
    case Color.BrightYellow  => Color.Yellow
    case Color.BrightBlue    => Color.Blue
    case Color.BrightMagenta => Color.Magenta
    case Color.BrightCyan    => Color.Cyan
    case Color.BrightWhite   => Color.White
    case other               => other

  private def brightOffset(c: Color): Int = c match
    case Color.BrightBlack   => 0
    case Color.BrightRed     => 1
    case Color.BrightGreen   => 2
    case Color.BrightYellow  => 3
    case Color.BrightBlue    => 4
    case Color.BrightMagenta => 5
    case Color.BrightCyan    => 6
    case Color.BrightWhite   => 7
    case _                   => 0

  private[tui] def styleToAnsi(s: Style, depth: ColorDepth): String =
    styleToAnsi(s, depth, extendedStyles = true)

  private[tui] def styleToAnsi(s: Style, depth: ColorDepth, extendedStyles: Boolean): String =
    val b = new StringBuilder
    if s.bold then b.append("\u001b[1m")
    if s.underline then b.append("\u001b[4m")
    if extendedStyles then
      // SGR ordering follows the canonical xterm sequence so terminals that
      // only honour a subset still see the supported codes contiguously.
      if s.dim then b.append("\u001b[2m")
      if s.italic then b.append("\u001b[3m")
      if s.blink then b.append("\u001b[5m")
      if s.reverse then b.append("\u001b[7m")
      if s.strikethrough then b.append("\u001b[9m")
    b.append(colorToAnsi(s.fg, isBg = false, depth))
    b.append(colorToAnsi(s.bg, isBg = true, depth))
    b.toString

  def clearPatch: String =
    ANSI.clearScreen

  def clear()(using terminal: TerminalBackend): Unit =
    terminal.write(clearPatch)

  def renderPatch(root: RootNode, depth: ColorDepth = ColorDepth.Ansi8): String =
    renderPatch(root, depth, extendedStyles = true)

  def renderPatch(root: RootNode, depth: ColorDepth, extendedStyles: Boolean): String =
    val out = new StringBuilder
    root.children.foreach(renderNode(_, out, depth, extendedStyles))
    layoutChildren(root).foreach(renderNode(_, out, depth, extendedStyles))
    if !hasModalOverlay(root) then root.input.foreach(renderInput(_, root.width, out, depth, extendedStyles))
    root.overlays.foreach(renderOverlay(_, root.width, root.height, out, depth, extendedStyles))
    activeOverlayInput(root).foreach(renderInput(_, root.width, out, depth, extendedStyles))
    out.toString

  /**
   * Resolve `root.layout` (if any) using the frame's `(width, height)` as
   * the available size budget. The resolution starts at `(1, 1)` so layouts
   * fill the whole frame; apps that want a margin should wrap their layout
   * in an outer `Column`/`Row` with a `Spacer`.
   */
  private def layoutChildren(root: RootNode): List[VNode] =
    root.layout match
      case Some(layout) =>
        Layout.resolveTo(
          layout,
          at = Coord(XCoord(1), YCoord(1)),
          availableWidth = math.max(0, root.width),
          availableHeight = math.max(0, root.height)
        )
      case None => Nil

  def render(root: RootNode)(using terminal: TerminalBackend): Unit =
    terminal.write(renderPatch(root, terminal.capabilities.colorDepth, terminal.capabilities.extendedStyles))

  /** Re-render only the input, leaving existing children intact. */
  def renderInputOnly(root: RootNode)(using terminal: TerminalBackend): Unit =
    terminal.write(inputPatch(root, terminal.capabilities.colorDepth, terminal.capabilities.extendedStyles))

  /** Build ANSI patch for input-only repaint. */
  def inputPatch(root: RootNode, depth: ColorDepth = ColorDepth.Ansi8): String =
    inputPatch(root, depth, extendedStyles = true)

  def inputPatch(root: RootNode, depth: ColorDepth, extendedStyles: Boolean): String =
    val out = new StringBuilder
    if hasModalOverlay(root) then
      activeOverlayInput(root).foreach(renderInput(_, root.width, out, depth, extendedStyles))
    else root.input.foreach(renderInput(_, root.width, out, depth, extendedStyles))
    out.toString

  /** True if any overlay in the root captures base-view input. */
  private def hasModalOverlay(root: RootNode): Boolean =
    root.overlays.exists(_.inputCapture == InputCapture.Modal)

  /**
   * Resolve which input the live cursor should follow:
   *   - the topmost modal overlay's input, if any
   *   - otherwise the topmost non-modal overlay's input, if any
   *   - otherwise None
   *
   * Each overlay-owned input has its coordinates translated into root-frame
   * absolutes by the time it is returned, so the existing input rendering
   * code can consume it without overlay awareness.
   */
  private def activeOverlayInput(root: RootNode): Option[InputNode] =
    val modal   = root.overlays.reverse.find(_.inputCapture == InputCapture.Modal)
    val visible = modal.orElse(root.overlays.reverse.find(_.input.isDefined))
    visible.flatMap(o => o.input.map(translateInput(o, root.width, root.height, _)))

  private def translateInput(o: Overlay, rootWidth: Int, rootHeight: Int, in: InputNode): InputNode =
    val (ox, oy) = OverlayPosition.resolve(o.position, o.width, o.height, rootWidth, rootHeight)
    in.copy(x = in.x + (ox.value - 1), y = in.y + (oy.value - 1))

  private def renderOverlay(
    o: Overlay,
    rootWidth: Int,
    rootHeight: Int,
    out: StringBuilder,
    depth: ColorDepth,
    extendedStyles: Boolean
  ): Unit =
    val (ox, oy) = OverlayPosition.resolve(o.position, o.width, o.height, rootWidth, rootHeight)
    val dx       = ox.value - 1
    val dy       = oy.value - 1
    // Wipe the overlay rectangle with spaces so the dialog interior is
    // opaque and panels behind don't bleed through.
    val blankRow = " " * o.width
    var row      = 0
    while row < o.height do
      out.append(moveTo(XCoord(ox.value), YCoord(oy.value + row))).append(blankRow)
      row += 1
    o.children.foreach(child => renderNode(Layout.translate(child, dx, dy), out, depth, extendedStyles))

  private def renderNode(v: VNode, out: StringBuilder, depth: ColorDepth, extendedStyles: Boolean): Unit =
    v match
      case TextNode(x, y, l) =>
        out.append(moveTo(x, y))
        l.foreach { case Text(str, style) =>
          out.append(styleToAnsi(style, depth, extendedStyles)).append(str).append(reset)
        }

      case BoxNode(x, y, w, h, children, style, chars) =>
        if style.border then drawBorder(x, y, w, h, style.fg, chars, out, depth)
        children.foreach(renderNode(_, out, depth, extendedStyles))

      case _: InputNode => () // handled separately

  private def drawBorder(
    x: XCoord,
    y: YCoord,
    w: Int,
    h: Int,
    color: Color,
    chars: BorderChars,
    out: StringBuilder,
    depth: ColorDepth
  ): Unit =
    val inner      = math.max(0, w - 2)
    val horizontal = chars.horizontal.toString * inner
    val v          = chars.vertical
    out
      .append(moveTo(x, y))
      .append(colorToAnsi(color, isBg = false, depth))
      .append(s"${chars.topLeft}$horizontal${chars.topRight}")
    (1 until (h - 1)).foreach(row => out.append(moveTo(x, y + row)).append(s"$v${" " * inner}$v"))
    out.append(moveTo(x, y + (h - 1))).append(s"${chars.bottomLeft}$horizontal${chars.bottomRight}")
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

  private def renderInput(
    inp: InputNode,
    rootWidth: Int,
    out: StringBuilder,
    depth: ColorDepth,
    extendedStyles: Boolean
  ): Unit =
    val visible = visibleInput(inp, rootWidth)

    // Clear full terminal row from column 1, then position to input x.
    // Some terminal emulators behave inconsistently when 2K is emitted away from column 1.
    out.append(moveTo(XCoord(1), inp.y))
    out.append("\u001b[2K")
    out.append(moveTo(inp.x, inp.y))

    val baseStyle = inp.style
    val baseAnsi  = styleToAnsi(baseStyle, depth, extendedStyles)
    // Draw the bounded single-line viewport only; never rely on terminal soft-wrap.
    out.append(baseAnsi).append(visible.text).append(reset)

    // Place the hardware cursor at the logical editing position.
    out.append(moveTo(inp.x + visible.cursorIndex, inp.y))

  /** Build a full terminal frame from a root node for diff-based rendering. */
  def buildFrame(root: RootNode): RenderFrame =
    def nodeExtents(node: VNode): (Int, Int) = node match
      case TextNode(x, y, segments) =>
        val textWidth = segments.foldLeft(0)((acc, seg) => acc + WCWidth.stringWidth(seg.txt))
        val right     = x.value + math.max(0, textWidth - 1)
        (right, y.value)

      case BoxNode(x, y, w, h, children, _, _) =>
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
      var i   = 0
      while i < str.length do
        val cp = str.codePointAt(i)
        val w  = WCWidth.codePointWidth(cp)
        val ch = str.charAt(i)
        if w == 2 then
          putCell(col, y, RenderCell(ch, style, 2))
          putCell(col + 1, y, RenderCell(' ', style, 0))
          col += 2
        else if w == 1 then
          putCell(col, y, RenderCell(ch, style, 1))
          col += 1
        // Combining marks / control / zero-width: skip silently. They would
        // ideally attach to the previous cell, but the cell model doesn't
        // carry composing-glyph slots yet — punting to a future iteration.
        i += java.lang.Character.charCount(cp)

    def drawBorder(x: Int, y: Int, w: Int, h: Int, style: Style, chars: BorderChars): Unit =
      if w > 0 && h > 0 then
        var dx = 0
        while dx < w do
          putCell(x + dx, y, RenderCell(chars.horizontal, style))
          if h > 1 then putCell(x + dx, y + h - 1, RenderCell(chars.horizontal, style))
          dx += 1

        var dy = 0
        while dy < h do
          putCell(x, y + dy, RenderCell(chars.vertical, style))
          if w > 1 then putCell(x + w - 1, y + dy, RenderCell(chars.vertical, style))
          dy += 1

        putCell(x, y, RenderCell(chars.topLeft, style))
        if w > 1 then putCell(x + w - 1, y, RenderCell(chars.topRight, style))
        if h > 1 then putCell(x, y + h - 1, RenderCell(chars.bottomLeft, style))
        if w > 1 && h > 1 then putCell(x + w - 1, y + h - 1, RenderCell(chars.bottomRight, style))

    def drawNode(node: VNode): Unit = node match
      case TextNode(x, y, segments) =>
        var col = x.value
        segments.foreach { case Text(str, style) =>
          drawString(col, y.value, str, style)
          col += WCWidth.stringWidth(str)
        }

      case BoxNode(x, y, w, h, children, style, chars) =>
        if style.border then drawBorder(x.value, y.value, w, h, Style(fg = style.fg), chars)
        children.foreach(drawNode)

      case _: InputNode =>
        ()

    root.children.foreach(drawNode)

    // Resolve any RootNode-level layout into the cell grid using the
    // frame's (width, height) as the size budget. Layouts that include
    // Layout.Fill children expand here at render time.
    layoutChildren(root).foreach(drawNode)

    val anyModalOverlay = root.overlays.exists(_.inputCapture == InputCapture.Modal)

    if !anyModalOverlay then
      root.input.foreach { inp =>
        val visible = visibleInput(inp, root.width)
        drawString(inp.x.value, inp.y.value, visible.text, inp.style)
        cursor = Some(Coord(inp.x + visible.cursorIndex, inp.y))
      }

    // Composite overlays bottom-to-top. Each overlay's children are
    // translated by the resolved position so callers author them in
    // overlay-local coordinates (1.x / 1.y is the overlay's top-left).
    //
    // Before drawing the children we wipe the overlay's rectangle with
    // blank cells so anything beneath is opaquely occluded — without this,
    // the box border draws but the interior leaks the panels behind it.
    root.overlays.foreach { o =>
      val (ox, oy) = OverlayPosition.resolve(o.position, o.width, o.height, root.width, root.height)
      val dx       = ox.value - 1
      val dy       = oy.value - 1
      var fy       = 0
      while fy < o.height do
        var fx = 0
        while fx < o.width do
          putCell(ox.value + fx, oy.value + fy, blankCell)
          fx += 1
        fy += 1
      o.children.foreach(child => drawNode(Layout.translate(child, dx, dy)))
      o.input.foreach { in =>
        val translated: InputNode = in.copy(x = in.x + dx, y = in.y + dy)
        val visible               = visibleInput(translated, root.width)
        drawString(translated.x.value, translated.y.value, visible.text, translated.style)
        cursor = Some(Coord(translated.x + visible.cursorIndex, translated.y))
      }
    }

    RenderFrame(width, height, cells, cursor)

  /** Diff two frames and emit only changed runs plus cursor movement. */
  def diff(prev: Option[RenderFrame], current: RenderFrame): DiffResult =
    diff(prev, current, ColorDepth.Ansi8, extendedStyles = true)

  /** Diff two frames at the given color depth. */
  def diff(prev: Option[RenderFrame], current: RenderFrame, depth: ColorDepth): DiffResult =
    diff(prev, current, depth, extendedStyles = true)

  /** Diff two frames honouring color depth and extended SGR capability. */
  def diff(
    prev: Option[RenderFrame],
    current: RenderFrame,
    depth: ColorDepth,
    extendedStyles: Boolean
  ): DiffResult =
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
            out.append(reset).append(styleToAnsi(style, depth, extendedStyles))
            var j = cursor
            while j < current.width &&
              cellAt(Some(current), row, j) != blankCell &&
              cellAt(Some(current), row, j).style == style
            do
              // Wide-char continuation cells (width = 0) carry the same
              // style as their leading wide cell but no glyph of their own
              // — the terminal already advanced its cursor past that
              // column when the wide glyph was emitted.
              if cellAt(Some(current), row, j).width != 0 then out.append(cellAt(Some(current), row, j).ch)
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
    diff(prev, current, ColorDepth.Ansi8, extendedStyles = true).ansi

  def renderDiff(prev: Option[RenderFrame], current: RenderFrame, depth: ColorDepth): String =
    diff(prev, current, depth, extendedStyles = true).ansi

  def renderDiff(
    prev: Option[RenderFrame],
    current: RenderFrame,
    depth: ColorDepth,
    extendedStyles: Boolean
  ): String =
    diff(prev, current, depth, extendedStyles).ansi

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
    val depth        = terminal.capabilities.colorDepth
    val ext          = terminal.capabilities.extendedStyles
    val initialDiff =
      if resized then AnsiRenderer.diff(None, currentFrame, depth, ext)
      else AnsiRenderer.diff(lastFrame, currentFrame, depth, ext)
    val shouldFullRepaint =
      resized || initialDiff.changedRows >= math.min(currentFrame.height, FullRepaintRowThreshold)
    val diffResult =
      if shouldFullRepaint then AnsiRenderer.diff(None, currentFrame, depth, ext)
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
