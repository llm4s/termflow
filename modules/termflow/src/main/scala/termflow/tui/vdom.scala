package termflow.tui

/**
 * Basic colour palette for text and borders.
 *
 * `Default` leaves the terminal's current foreground/background untouched
 * and is the idiomatic way to say "no explicit colour" — it is not emitted
 * as an ANSI escape at render time.
 */
enum Color:
  case Default, Black, Red, Green, Yellow, Blue, Magenta, Cyan, White

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
   * renderer draws a border from `(x, y)` to `(x + width - 1, y + height - 1)`.
   * Children are drawn in document order on top of the box; they are not
   * automatically clipped or repositioned relative to the box.
   *
   * @param x The 1-based column of the top-left corner.
   * @param y The 1-based row of the top-left corner.
   * @param width The box width including both border columns.
   * @param height The box height including both border rows.
   * @param children Child nodes drawn inside / over the box.
   * @param style The box style. `Style(border = true)` is the usual setting.
   */
  case BoxNode(
    override val x: XCoord,
    override val y: YCoord,
    override val width: Int,
    override val height: Int,
    children: List[VNode],
    override val style: Style = Style()
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
    case BoxNode(x, _, _, _, _, _)      => x
    case InputNode(x, _, _, _, _, _, _) => x

  /** Top-left row of this node (1-based). */
  def y: YCoord = this match
    case TextNode(_, y, _)              => y
    case BoxNode(_, y, _, _, _, _)      => y
    case InputNode(_, y, _, _, _, _, _) => y

  /**
   * Width of this node in cells.
   *
   * `TextNode` always reports `1` — it does not know its own visible width
   * because that depends on the underlying `Text` segments. Use
   * `AnsiRenderer.buildFrame` for authoritative layout.
   */
  def width: Int = this match
    case TextNode(_, _, _)             => 1
    case BoxNode(_, _, width, _, _, _) => width
    case InputNode(_, _, prompt, _, _, lineWidth, _) =>
      if lineWidth > 0 then lineWidth else prompt.length + 1

  /** Height of this node in cells. Always `1` for text and input nodes. */
  def height: Int = this match
    case TextNode(_, _, _)              => 1
    case BoxNode(_, _, _, height, _, _) => height
    case InputNode(_, _, _, _, _, _, _) => 1

  /** The style applied to this node. [[TextNode]] styles live on its segments. */
  def style: Style = this match
    case TextNode(_, _, _)                  => Style()
    case BoxNode(_, _, _, _, _, style)      => style
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
 * @param width Frame width in cells.
 * @param height Frame height in cells.
 * @param children Static nodes to draw, in document order.
 * @param input Optional focused [[InputNode]] rendered last, with cursor placement.
 */
final case class RootNode(
  width: Int,
  height: Int,
  children: List[VNode],
  input: Option[InputNode]
)
