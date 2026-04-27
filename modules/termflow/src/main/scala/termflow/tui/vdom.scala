package termflow.tui

/**
 * Color used for text and borders.
 *
 * Three families are available:
 *   - the legacy 8-color palette ([[Color.Default]] through [[Color.White]])
 *     plus the bright variants ([[Color.BrightBlack]] through
 *     [[Color.BrightWhite]]) for terminals advertising `Ansi16` or higher;
 *   - [[Color.Indexed]] for the xterm 256-color cube;
 *   - [[Color.Rgb]] for 24-bit truecolor.
 *
 * The renderer downgrades higher-depth colors to whatever the active
 * [[Capabilities.colorDepth]] supports — `Color.Rgb(255, 100, 100)` is
 * rendered as a near-equivalent indexed color on a 256-color terminal and
 * as the closest of the 8 standard colors on a basic ANSI terminal.
 *
 * [[Color.Default]] leaves the terminal's current foreground/background
 * untouched and emits no escape sequence.
 */
enum Color:
  case Default
  case Black, Red, Green, Yellow, Blue, Magenta, Cyan, White
  case BrightBlack, BrightRed, BrightGreen, BrightYellow,
    BrightBlue, BrightMagenta, BrightCyan, BrightWhite

  /**
   * 256-color palette index, 0..255. Values outside the range are clamped
   * at render time. Indices 0..15 mirror the 16-color palette; 16..231 form
   * a 6×6×6 RGB cube; 232..255 are a 24-step grayscale ramp.
   */
  case Indexed(n: Int)

  /**
   * 24-bit truecolor. Components outside 0..255 are clamped at render time.
   */
  case Rgb(r: Int, g: Int, b: Int)

object Color:

  /**
   * Approximate sRGB representation of each named color. Used only for
   * downgrade math (truecolor → 8/16). The exact rendered color is up to
   * the terminal's palette settings.
   */
  private[tui] val namedRgb: Map[Color, (Int, Int, Int)] = Map(
    Color.Black         -> (0, 0, 0),
    Color.Red           -> (170, 0, 0),
    Color.Green         -> (0, 170, 0),
    Color.Yellow        -> (170, 85, 0),
    Color.Blue          -> (0, 0, 170),
    Color.Magenta       -> (170, 0, 170),
    Color.Cyan          -> (0, 170, 170),
    Color.White         -> (170, 170, 170),
    Color.BrightBlack   -> (85, 85, 85),
    Color.BrightRed     -> (255, 85, 85),
    Color.BrightGreen   -> (85, 255, 85),
    Color.BrightYellow  -> (255, 255, 85),
    Color.BrightBlue    -> (85, 85, 255),
    Color.BrightMagenta -> (255, 85, 255),
    Color.BrightCyan    -> (85, 255, 255),
    Color.BrightWhite   -> (255, 255, 255)
  )

  /**
   * Resolve any non-`Default` color to an approximate `(r, g, b)`. Used by
   * the renderer when downgrading to a lower color depth.
   *
   * Returns `None` for [[Color.Default]] — Default is not a color, it is
   * "leave whatever the terminal already has".
   */
  private[tui] def toRgb(c: Color): Option[(Int, Int, Int)] = c match
    case Color.Default                     => None
    case named if namedRgb.contains(named) => namedRgb.get(named)
    case Color.Indexed(n)                  => Some(indexedToRgb(clamp(n, 0, 255)))
    case Color.Rgb(r, g, b)                => Some((clamp(r, 0, 255), clamp(g, 0, 255), clamp(b, 0, 255)))
    case _                                 => None

  /**
   * Map a 256-color index to its approximate RGB triple, mirroring the
   * standard xterm palette layout.
   */
  private[tui] def indexedToRgb(n: Int): (Int, Int, Int) =
    if n < 16 then
      val named = Color.fromOrdinal(n + 1) // skip Default at ordinal 0
      namedRgb.getOrElse(named, (0, 0, 0))
    else if n < 232 then
      val nn                = n - 16
      val r                 = nn / 36
      val g                 = (nn % 36) / 6
      val b                 = nn  % 6
      def step(c: Int): Int = if c == 0 then 0 else 55 + c * 40
      (step(r), step(g), step(b))
    else
      val v = 8 + (n - 232) * 10
      (v, v, v)

  /**
   * Find the nearest color in the 6×6×6 cube + grayscale ramp (16..255).
   * Considers both the cube and the grayscale ramp and returns whichever
   * is closer in straight RGB space.
   */
  private[tui] def nearestIndexed(r: Int, g: Int, b: Int): Int =
    val cubeIdx      = nearestCube(r, g, b)
    val (cr, cg, cb) = indexedToRgb(cubeIdx)
    val cubeDist     = squaredDistance((r, g, b), (cr, cg, cb))

    val grayIdx      = nearestGray(r, g, b)
    val (gr, gg, gb) = indexedToRgb(grayIdx)
    val grayDist     = squaredDistance((r, g, b), (gr, gg, gb))

    if grayDist < cubeDist then grayIdx else cubeIdx

  private def nearestCube(r: Int, g: Int, b: Int): Int =
    def quantize(c: Int): Int =
      // xterm cube steps: 0, 95, 135, 175, 215, 255
      val steps = Array(0, 95, 135, 175, 215, 255)
      var best  = 0
      var bestD = Int.MaxValue
      var i     = 0
      while i < steps.length do
        val d = math.abs(c - steps(i))
        if d < bestD then { bestD = d; best = i }
        i += 1
      best
    16 + 36 * quantize(r) + 6 * quantize(g) + quantize(b)

  private def nearestGray(r: Int, g: Int, b: Int): Int =
    val avg  = (r + g + b) / 3
    val gray = clamp(((avg - 8) + 5) / 10, 0, 23)
    232 + gray

  /** Find the nearest of the 8 base colors (Black..White). */
  private[tui] def nearestAnsi8(r: Int, g: Int, b: Int): Color =
    val candidates: Seq[Color] =
      Seq(Color.Black, Color.Red, Color.Green, Color.Yellow, Color.Blue, Color.Magenta, Color.Cyan, Color.White)
    candidates.minBy(c => squaredDistance((r, g, b), namedRgb(c)))

  /** Find the nearest of the 16 standard colors (8 base + 8 bright). */
  private[tui] def nearestAnsi16(r: Int, g: Int, b: Int): Color =
    namedRgb.keys.toSeq.minBy(c => squaredDistance((r, g, b), namedRgb(c)))

  private def squaredDistance(a: (Int, Int, Int), b: (Int, Int, Int)): Int =
    val dr = a._1 - b._1
    val dg = a._2 - b._2
    val db = a._3 - b._3
    dr * dr + dg * dg + db * db

  private def clamp(v: Int, lo: Int, hi: Int): Int = math.max(lo, math.min(hi, v))

/**
 * Styling applied to a cell or run of cells.
 *
 * Styles are combined with the character content at render time by
 * `AnsiRenderer` and emitted as SGR escape sequences. A default-constructed
 * `Style` produces no escape sequences at all, so "plain text" stays plain.
 *
 * @param fg Foreground colour. [[Color.Default]] leaves it unset.
 * @param bg Background colour. [[Color.Default]] leaves it unset.
 * @param bold Enable the bold attribute.
 * @param underline Enable the underline attribute.
 * @param border Only meaningful on a [[BoxNode]]: draw a border around the box.
 */
final case class Style(
  fg: Color = Color.Default,
  bg: Color = Color.Default,
  bold: Boolean = false,
  underline: Boolean = false,
  border: Boolean = false
)

/**
 * A run of text with an associated [[Style]].
 *
 * Multiple `Text` segments can be concatenated inside a single [[TextNode]]
 * to mix styles on one row without having to lay out multiple nodes:
 *
 * {{{
 * TextNode(2.x, 3.y, List(
 *   "Status: ".text,
 *   "OK".text(fg = Color.Green, bold = true)
 * ))
 * }}}
 */
final case class Text(txt: String, style: Style)

/**
 * Virtual-DOM node type.
 *
 * All coordinates are 1-based (mirroring the underlying ANSI protocol) and
 * absolute — there is no layout pass at the moment, so parents do not affect
 * child positions. See issue #77 for the planned flex/stack layout engine.
 *
 * The three variants cover everything the current renderer supports:
 *   - [[VNode.TextNode]] — styled text at a fixed coordinate
 *   - [[VNode.BoxNode]]  — container with an optional border
 *   - [[VNode.InputNode]] — single-line editable prompt with a cursor
 */
enum VNode:

  /**
   * A run of styled text anchored at `(x, y)`.
   *
   * @param x The 1-based column of the first character.
   * @param y The 1-based row.
   * @param txt One or more [[Text]] segments, concatenated left-to-right.
   */
  case TextNode(
    override val x: XCoord,
    override val y: YCoord,
    txt: List[Text]
  )

  /**
   * A rectangular container anchored at `(x, y)`.
   *
   * `BoxNode` is currently visual-only: if `style.border` is set, the
   * renderer draws a border from `(x, y)` to `(x + width - 1, y + height - 1)`
   * using the glyphs defined by `chars` (defaults to [[BorderChars.sharp]]).
   * Children are drawn in document order on top of the box; they are not
   * automatically clipped or repositioned relative to the box.
   *
   * @param x The 1-based column of the top-left corner.
   * @param y The 1-based row of the top-left corner.
   * @param width The box width including both border columns.
   * @param height The box height including both border rows.
   * @param children Child nodes drawn inside / over the box.
   * @param style The box style. `Style(border = true)` is the usual setting.
   * @param chars Box-drawing glyphs used when the border is on.
   *              Pick one of [[BorderChars.sharp]] / [[BorderChars.rounded]] /
   *              [[BorderChars.double]] / [[BorderChars.ascii]] or roll your own.
   */
  case BoxNode(
    override val x: XCoord,
    override val y: YCoord,
    override val width: Int,
    override val height: Int,
    children: List[VNode],
    override val style: Style = Style(),
    chars: BorderChars = BorderChars.sharp
  )

  /**
   * A single-line editable prompt with a cursor.
   *
   * `InputNode` is the only node type that the renderer treats as "live":
   * after drawing the static content it moves the hardware cursor into the
   * input at `cursor`, so editing is reflected at the terminal level.
   *
   * @param x 1-based column of the first character of the prompt.
   * @param y 1-based row of the prompt.
   * @param prompt The buffer to display (already rendered from `Prompt.State`).
   * @param style The style applied to the prompt text.
   * @param cursor Cursor index within `prompt`. `-1` means "at end".
   * @param lineWidth Fixed viewport width. `0` means "use the prompt length".
   * @param prefixLength Number of leading characters that belong to a fixed
   *                     prefix (e.g. `">>> "`) and should be pinned to the
   *                     left edge even when the viewport scrolls.
   */
  case InputNode(
    override val x: XCoord,
    override val y: YCoord,
    prompt: String,
    override val style: Style,
    cursor: Int = -1,
    lineWidth: Int = 0,
    prefixLength: Int = 0
  )

  /** Top-left column of this node (1-based). */
  def x: XCoord = this match
    case TextNode(x, _, _)              => x
    case BoxNode(x, _, _, _, _, _, _)   => x
    case InputNode(x, _, _, _, _, _, _) => x

  /** Top-left row of this node (1-based). */
  def y: YCoord = this match
    case TextNode(_, y, _)              => y
    case BoxNode(_, y, _, _, _, _, _)   => y
    case InputNode(_, y, _, _, _, _, _) => y

  /**
   * Width of this node in cells.
   *
   * `TextNode` always reports `1` — it does not know its own visible width
   * because that depends on the underlying `Text` segments. Use
   * `AnsiRenderer.buildFrame` for authoritative layout.
   */
  def width: Int = this match
    case TextNode(_, _, _)                => 1
    case BoxNode(_, _, width, _, _, _, _) => width
    case InputNode(_, _, prompt, _, _, lineWidth, _) =>
      if lineWidth > 0 then lineWidth else prompt.length + 1

  /** Height of this node in cells. Always `1` for text and input nodes. */
  def height: Int = this match
    case TextNode(_, _, _)                 => 1
    case BoxNode(_, _, _, height, _, _, _) => height
    case InputNode(_, _, _, _, _, _, _)    => 1

  /** The style applied to this node. [[TextNode]] styles live on its segments. */
  def style: Style = this match
    case TextNode(_, _, _)                  => Style()
    case BoxNode(_, _, _, _, _, style, _)   => style
    case InputNode(_, _, _, style, _, _, _) => style

/**
 * Compatibility alias so call sites can reference `TextNode` unqualified
 * instead of `VNode.TextNode`. Matches the spelling used by the sample apps.
 */
type TextNode = VNode.TextNode
val TextNode = VNode.TextNode

/** Compatibility alias; see [[TextNode]]. */
type BoxNode = VNode.BoxNode
val BoxNode = VNode.BoxNode

/** Compatibility alias; see [[TextNode]]. */
type InputNode = VNode.InputNode
val InputNode = VNode.InputNode

/**
 * The top-level virtual-DOM container produced by `TuiApp.view`.
 *
 * `RootNode` declares the logical frame size and a list of children plus an
 * optional focused input. The actual terminal width/height is read from the
 * backend by the renderer; the `width`/`height` here are the app's own idea
 * of the drawing surface and are used by `AnsiRenderer.buildFrame` to size
 * the cell matrix.
 *
 * @param width    Frame width in cells.
 * @param height   Frame height in cells.
 * @param children Static nodes to draw, in document order.
 * @param input    Optional focused [[InputNode]] rendered last, with cursor placement.
 * @param overlays Optional stack of [[Overlay]]s composited on top of the base
 *                 view, in document order (bottom-to-top z-order). When any
 *                 overlay sets `inputCapture = InputCapture.Modal`, the base
 *                 view's `input` is suppressed and the topmost modal overlay's
 *                 `input` (if any) takes over the hardware cursor.
 * @param layout   Optional [[Layout]] resolved by the renderer at render
 *                 time using the frame's `(width, height)` as the size
 *                 budget. Distinct from `children` (positioned-node
 *                 authoring): apps using `layout` get reflow-on-resize and
 *                 `Layout.Fill` semantics for free. Both `children` and
 *                 `layout` may be present — `children` paints first, then
 *                 the resolved layout.
 */
final case class RootNode(
  width: Int,
  height: Int,
  children: List[VNode],
  input: Option[InputNode],
  overlays: List[Overlay] = Nil,
  layout: Option[Layout] = None
)
