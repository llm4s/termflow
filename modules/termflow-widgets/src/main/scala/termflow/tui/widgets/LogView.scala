package termflow.tui.widgets

import termflow.tui.*

/**
 * Append-only scrollback viewer for log lines, REPL output, chat
 * transcripts, and similar streams.
 *
 * The widget renders a fixed-size viewport into a longer line buffer.
 * Apps own the buffer (typically a bounded `Vector[String]` or a
 * ring-buffer) and a `scrollOffset` counted in *display lines* from the
 * bottom — `0` is "tail of the buffer pinned to the last row" (the
 * common live-feed view), positive values scroll back into history.
 *
 * Long lines are wrapped to fit `width` when `wrap = true` (default),
 * or truncated with a trailing `…` when `wrap = false`. Wrapping is
 * computed by [[expand]] (which delegates per line to [[wrapLine]]);
 * the visible window is sliced by [[viewport]].
 * Both helpers are public so apps can pre-compute display lines once
 * per buffer change instead of every frame.
 *
 * {{{
 * given Theme = Theme.dark
 * Layout.column(gap = 0)(
 *   LogView(
 *     lines        = chatBuffer,
 *     width        = 60,
 *     height       = 12,
 *     scrollOffset = 0   // pinned to live tail
 *   ): _*
 * )
 * }}}
 *
 * @param lines        The raw line buffer. The widget does not mutate
 *                     it.
 * @param width        Cell width of the viewport.
 * @param height       Cell height (number of rows rendered).
 * @param scrollOffset Number of display lines scrolled up from the
 *                     tail. `0` keeps the latest line on the last row.
 *                     Negative values are treated as `0`.
 * @param at           Top-left cell of the viewport.
 * @param wrap         Whether to wrap long lines to fit `width` (default
 *                     true) or truncate with `…`.
 * @param style        Optional style override for the rendered rows.
 *                     Defaults to the theme's `foreground` slot.
 */
object LogView:

  /** Truncation glyph used when `wrap = false`. */
  private val ellipsis = "…"

  def apply(
    lines: Seq[String],
    width: Int,
    height: Int,
    scrollOffset: Int = 0,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    wrap: Boolean = true,
    style: Option[Style] = None
  )(using theme: Theme): List[VNode] =
    val effectiveWidth  = math.max(1, width)
    val effectiveHeight = math.max(0, height)
    val displayLines    = expand(lines, effectiveWidth, wrap)
    val visible         = viewport(displayLines, effectiveHeight, scrollOffset)
    val rowStyle        = style.getOrElse(Style(fg = theme.foreground))
    visible.zipWithIndex.toList.map { case (line, i) =>
      TextNode(at.x, at.y + i, List(Text(line, rowStyle)))
    }

  /**
   * Expand a raw line buffer into the display-line representation that
   * the viewport will scroll over. Wrapped lines become multiple
   * display lines; truncated lines become one. Empty input lines stay
   * as empty display lines.
   */
  def expand(lines: Seq[String], width: Int, wrap: Boolean): Vector[String] =
    val w = math.max(1, width)
    lines.foldLeft(Vector.empty[String]) { (acc, line) =>
      if wrap then acc ++ wrapLine(line, w)
      else acc :+ truncateLine(line, w)
    }

  /** Wrap a single line into chunks of at most `width` columns. */
  def wrapLine(line: String, width: Int): Vector[String] =
    val w = math.max(1, width)
    if line.isEmpty then Vector("")
    else
      val builder = Vector.newBuilder[String]
      var i       = 0
      while i < line.length do
        val end = math.min(line.length, i + w)
        builder += line.substring(i, end)
        i = end
      builder.result()

  /** Truncate a single line to `width`, appending `…` if it doesn't fit. */
  def truncateLine(line: String, width: Int): String =
    val w = math.max(1, width)
    if line.length <= w then line
    else if w == 1 then ellipsis
    else line.substring(0, w - 1) + ellipsis

  /**
   * Slice `displayLines` into the visible window: `height` lines anchored
   * at `scrollOffset` from the tail. The result is padded on top with
   * blank rows when the buffer is shorter than the viewport, so apps can
   * draw a fixed-height region without conditional logic.
   */
  def viewport(displayLines: Seq[String], height: Int, scrollOffset: Int): Vector[String] =
    val h     = math.max(0, height)
    val total = displayLines.size
    if h == 0 then Vector.empty
    else
      val offset    = math.max(0, math.min(math.max(0, total - h), scrollOffset))
      val tailIndex = math.max(0, total - h - offset)
      val available = displayLines.slice(tailIndex, tailIndex + h).toVector
      val pad       = h - available.size
      if pad > 0 then Vector.fill(pad)("") ++ available
      else available

  /** Number of display lines `lines` would expand to at this width. */
  def displayLineCount(lines: Seq[String], width: Int, wrap: Boolean): Int =
    expand(lines, width, wrap).size

  /**
   * Maximum scroll offset for `lines` at this viewport size. Apps can
   * use this to clamp scroll input from arrow keys or wheel events.
   */
  def maxScroll(lines: Seq[String], width: Int, height: Int, wrap: Boolean): Int =
    math.max(0, displayLineCount(lines, width, wrap) - math.max(1, height))

  /**
   * Map a `MouseEvent.Scroll` to a `LogView` scroll delta (negative for
   * up / older lines, positive for down / newer lines), provided the
   * scroll happened inside the viewport rectangle. Horizontal scroll
   * (`Left` / `Right`) is ignored.
   *
   * Apps wire this into their `update`:
   *
   * {{{
   * case Msg.Key(InputKey.Mouse(ev)) =>
   *   val viewportRect = LogView.Viewport(at, width, height)
   *   LogView.scrollDelta(ev, viewportRect) match
   *     case Some(d) => Tui(scrollBy(model, d))
   *     case None    => model.tui
   * }}}
   *
   * @param event           The decoded mouse event.
   * @param viewport        Viewport rectangle the scroll must land in.
   * @param ticksPerDetent  Lines moved per wheel detent. Defaults to
   *                        `3`, which roughly matches the OS-level
   *                        scroll-acceleration users expect from a real
   *                        terminal log pane. Pass `1` for one-line-per-
   *                        detent semantics.
   * @return `Some(delta)` if the event is a vertical scroll inside the
   *         viewport, `None` otherwise.
   */
  def scrollDelta(
    event: MouseEvent,
    viewport: Viewport,
    ticksPerDetent: Int = 3
  ): Option[Int] =
    event match
      case MouseEvent.Scroll(direction, col, row, _) if viewport.contains(col, row) =>
        val step = math.max(1, ticksPerDetent)
        direction match
          case ScrollDirection.Up    => Some(-step)
          case ScrollDirection.Down  => Some(+step)
          case ScrollDirection.Left  => None
          case ScrollDirection.Right => None
      case _ => None

  /**
   * Rectangle in absolute terminal cells the viewport occupies. Used by
   * [[scrollDelta]] to decide whether a mouse-wheel event should drive
   * the LogView; sized identically to the `width` / `height` / `at`
   * arguments passed to `LogView.apply`.
   */
  final case class Viewport(at: Coord, width: Int, height: Int):
    private val left: Int   = at.x.value
    private val top: Int    = at.y.value
    private val right: Int  = left + math.max(0, width) - 1
    private val bottom: Int = top + math.max(0, height) - 1

    /** True if the absolute `(col, row)` lies within the viewport. */
    def contains(col: Int, row: Int): Boolean =
      col >= left && col <= right && row >= top && row <= bottom
