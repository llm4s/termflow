package termflow.tui.widgets

import termflow.tui.*

/**
 * Two-pane layout helper.
 *
 * Splits a rectangular region into two panes side-by-side
 * ([[Direction.Horizontal]]) or stacked ([[Direction.Vertical]]) at a
 * configurable `splitRatio`. Apps supply two render functions that
 * receive the resolved `(at, width, height)` of their pane and return
 * the `VNode`s to draw into it.
 *
 * Mouse-drag to resize the divider is not yet implemented — that needs
 * the layout-pass hit-test cache (deferred to Stage 3 mouse follow-up).
 * Until then apps drive `splitRatio` via their own keymap (or simply
 * leave it fixed). Scaffolding for the divider rect is exposed via
 * [[dividerRect]] so apps that want to wire mouse drag themselves can.
 *
 * {{{
 * given Theme = Theme.dark
 *
 * SplitPane(
 *   direction  = SplitPane.Direction.Horizontal,
 *   width      = 80,
 *   height     = 24,
 *   splitRatio = 0.4,
 *   first      = (at, w, h) => leftPane(at, w, h),
 *   second     = (at, w, h) => rightPane(at, w, h)
 * )
 * }}}
 *
 * @param first      Renderer for the first pane (left or top).
 * @param second     Renderer for the second pane (right or bottom).
 * @param width      Total cell width of the splittable region.
 * @param height     Total cell height of the splittable region.
 * @param direction  [[Direction.Horizontal]] (side-by-side) or
 *                   [[Direction.Vertical]] (stacked). Default horizontal.
 * @param at         Top-left cell of the region. Defaults to `(1, 1)`.
 * @param splitRatio Fraction of the major axis allocated to the first
 *                   pane, clamped to `[minSizeRatio, 1 - minSizeRatio]`.
 *                   Defaults to `0.5`.
 * @param gap        Cells reserved between the two panes for the
 *                   divider. `0` means flush.
 */
object SplitPane:

  /** Direction of the split. */
  enum Direction:
    case Horizontal // first | second
    case Vertical   // first
    // ─────
    // second

  /**
   * Both panes are guaranteed to be at least this fraction of the
   *  major axis. Stops `splitRatio = 0.0 / 1.0` from collapsing one
   *  pane to nothing.
   */
  val MinSizeRatio: Double = 0.05

  /**
   * A pane's resolved rectangle. Returned from [[layout]] for apps
   *  that want to compute hit-tests outside the widget.
   */
  final case class Pane(at: Coord, width: Int, height: Int)

  /**
   * First / second / divider rectangles for the given configuration.
   *  Pure — no rendering side-effects.
   */
  final case class Layout(first: Pane, second: Pane, divider: Option[Pane])

  /**
   * Resolve the split into pane rectangles. Apps that need to map
   * mouse coordinates to a pane (or to the divider for drag-resize)
   * call this once per frame and reuse the result.
   */
  def layout(
    direction: Direction,
    width: Int,
    height: Int,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    splitRatio: Double = 0.5,
    gap: Int = 0
  ): Layout =
    val major =
      if direction == Direction.Horizontal then math.max(0, width)
      else math.max(0, height)
    val gapClamped = math.max(0, math.min(gap, math.max(0, major - 2)))
    val available  = math.max(0, major - gapClamped)
    val ratio      = math.max(MinSizeRatio, math.min(1.0 - MinSizeRatio, splitRatio))
    val firstSize  = math.max(1, (available * ratio).round.toInt)
    val secondSize = math.max(1, available - firstSize)

    direction match
      case Direction.Horizontal =>
        val firstPane   = Pane(at, firstSize, height)
        val dividerPane = if gapClamped > 0 then Some(Pane(at + (firstSize, 0), gapClamped, height)) else None
        val secondAt    = at + (firstSize + gapClamped, 0)
        val secondPane  = Pane(secondAt, secondSize, height)
        Layout(firstPane, secondPane, dividerPane)
      case Direction.Vertical =>
        val firstPane   = Pane(at, width, firstSize)
        val dividerPane = if gapClamped > 0 then Some(Pane(at + (0, firstSize), width, gapClamped)) else None
        val secondAt    = at + (0, firstSize + gapClamped)
        val secondPane  = Pane(secondAt, width, secondSize)
        Layout(firstPane, secondPane, dividerPane)

  /**
   * Resolved divider rectangle, if `gap > 0`. Convenience for callers
   *  that only care about the drag region.
   */
  def dividerRect(
    direction: Direction,
    width: Int,
    height: Int,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    splitRatio: Double = 0.5,
    gap: Int = 0
  ): Option[Pane] =
    layout(direction, width, height, at, splitRatio, gap).divider

  /**
   * Render both panes. The renderer functions are called with the
   * resolved `(at, width, height)` of their pane.
   */
  def apply(
    first: (Coord, Int, Int) => List[VNode],
    second: (Coord, Int, Int) => List[VNode],
    width: Int,
    height: Int,
    direction: Direction = Direction.Horizontal,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    splitRatio: Double = 0.5,
    gap: Int = 0
  ): List[VNode] =
    val l = layout(direction, width, height, at, splitRatio, gap)
    first(l.first.at, l.first.width, l.first.height) ++
      second(l.second.at, l.second.width, l.second.height)

  // Coord arithmetic for the local layout math.
  extension (c: Coord) private def +(d: (Int, Int)): Coord = Coord(c.x + d._1, c.y + d._2)
