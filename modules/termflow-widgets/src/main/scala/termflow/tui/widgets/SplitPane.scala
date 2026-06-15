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
 * Mouse-drag to resize the divider is supported via the [[handleMouse]]
 * companion: hold a [[DragState]] in the app's model, feed each
 * `MouseEvent` (typically extracted from `InputKey.Mouse(...)`) through
 * `handleMouse`, and use the returned `splitRatio` when constructing
 * the next frame. The divider rectangle is exposed via [[dividerRect]]
 * so apps that want to wire their own drag handling can; `gap >= 1` is
 * required for the divider to be pickable.
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
    // Cap firstSize at `available - 1` so the second pane always has room and
    // `firstSize + gap + secondSize == major` holds exactly. Previously both
    // sizes independently floored at 1, so for a small region the panes summed
    // to more than `available` and the second pane (and divider) spilled past
    // the region edge. When `available < 2` there is no room for two non-empty
    // panes, so the surplus pane resolves to width/height 0 rather than
    // overflowing.
    val firstSize =
      if available <= 1 then math.min(available, 1)
      else math.max(1, math.min(available - 1, (available * ratio).round.toInt))
    val secondSize = math.max(0, available - firstSize)

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
   * Drag state for mouse-driven divider resize.
   *
   * Apps hold a single `DragState` per `SplitPane`, default
   * [[DragState.idle]], and feed every `KeyDecoder.InputKey.Mouse`
   * event into [[handleMouse]]. The `splitRatio` field is what the
   * next frame should use; `dragging` is true while the user is
   * mid-gesture so the app can render a focused divider style if
   * it likes.
   *
   * @param splitRatio Current split ratio. Always clamped to
   *                   `[MinSizeRatio, 1 - MinSizeRatio]`.
   * @param dragging   True between a `MouseEvent.Press` on the divider
   *                   and the next `MouseEvent.Release`.
   */
  final case class DragState(splitRatio: Double, dragging: Boolean):
    /** Reset back to the idle state at the supplied ratio. */
    def reset(ratio: Double): DragState = DragState(clampRatio(ratio), dragging = false)

  object DragState:
    /** Idle drag state at `0.5`. */
    val idle: DragState = DragState(0.5, dragging = false)

    /** Idle drag state at the supplied ratio (clamped). */
    def at(ratio: Double): DragState = DragState(clampRatio(ratio), dragging = false)

  /**
   * Feed a mouse event into a [[DragState]] and return the updated
   * state. Apps wire this from their `update` handler:
   *
   * {{{
   * case Msg.Mouse(ev) =>
   *   val drag = SplitPane.handleMouse(
   *     state = m.drag,
   *     event = ev,
   *     direction = SplitPane.Direction.Horizontal,
   *     width = m.width, height = m.height,
   *     gap = 1
   *   )
   *   m.copy(drag = drag).tui
   * }}}
   *
   * Press on the divider rect arms the drag; subsequent Drag events
   * update the ratio based on the cursor's position along the major
   * axis; Release ends the gesture. Events outside the divider while
   * not dragging are ignored.
   *
   * `gap` must be >= 1 for the divider to be pickable; with `gap = 0`
   * there is nowhere to click.
   *
   * @param state     Current drag state.
   * @param event     Mouse event to process.
   * @param direction Split direction.
   * @param width     Total cell width of the splittable region.
   * @param height    Total cell height of the splittable region.
   * @param at        Top-left of the splittable region. Defaults to `(1, 1)`.
   * @param gap       Divider gap in cells. Must be >= 1 for drag.
   */
  def handleMouse(
    state: DragState,
    event: MouseEvent,
    direction: Direction,
    width: Int,
    height: Int,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    gap: Int = 1
  ): DragState =
    if gap < 1 then state
    else
      val l       = layout(direction, width, height, at, state.splitRatio, gap)
      val divider = l.divider
      event match
        case MouseEvent.Press(MouseButton.Left, col, row, _) =>
          if divider.exists(_.contains(col, row)) then state.copy(dragging = true)
          else state

        case MouseEvent.Drag(MouseButton.Left, col, row, _) if state.dragging =>
          state.copy(splitRatio = ratioAt(direction, width, height, at, col, row))

        case MouseEvent.Release(MouseButton.Left, col, row, _) =>
          if state.dragging then
            // If the release lands inside the splittable region, snap to the
            // pointer's final ratio; otherwise leave the ratio where the last
            // Drag put it.
            val withinRegion = isInside(width, height, at, col, row)
            val ratio = if withinRegion then ratioAt(direction, width, height, at, col, row) else state.splitRatio
            DragState(clampRatio(ratio), dragging = false)
          else state

        case _ => state

  private def isInside(
    width: Int,
    height: Int,
    at: Coord,
    col: Int,
    row: Int
  ): Boolean =
    val left = at.x.value
    val top  = at.y.value
    col >= left && col < left + width && row >= top && row < top + height

  private def ratioAt(
    direction: Direction,
    width: Int,
    height: Int,
    at: Coord,
    col: Int,
    row: Int
  ): Double =
    // Map the pointer onto a symmetric raw ratio in [0, 1]: the first and
    // last cells of the region map to exactly 0 and 1, so the reachable
    // range is symmetric about the centre (0.5) before `clampRatio`. Using
    // `span - 1` as the denominator (rather than `span`, with a `+1` bias)
    // is what makes far-left and far-right drags mirror images.
    direction match
      case Direction.Horizontal =>
        val span    = math.max(1, width)
        val denom   = math.max(1, span - 1)
        val rel     = col - at.x.value // 0..width-1 inside the region
        val clamped = math.max(0, math.min(span - 1, rel))
        clamped.toDouble / denom
      case Direction.Vertical =>
        val span    = math.max(1, height)
        val denom   = math.max(1, span - 1)
        val rel     = row - at.y.value // 0..height-1 inside the region
        val clamped = math.max(0, math.min(span - 1, rel))
        clamped.toDouble / denom

  private def clampRatio(ratio: Double): Double =
    math.max(MinSizeRatio, math.min(1.0 - MinSizeRatio, ratio))

  /** Extension for [[Pane]] hit-testing — used by [[handleMouse]]. */
  extension (pane: Pane)
    private def contains(col: Int, row: Int): Boolean =
      val left = pane.at.x.value
      val top  = pane.at.y.value
      col >= left && col < left + pane.width &&
      row >= top && row < top + pane.height

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
