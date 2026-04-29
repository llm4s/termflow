package termflow.tui.widgets

import termflow.tui.*

/**
 * Vertical or horizontal scroll indicator.
 *
 * Renders a track (filled with the theme's `secondary` slot) plus a
 * proportional thumb (the theme's `primary` slot). The thumb size is a
 * fraction of the track corresponding to `visible / total`, and its
 * position corresponds to `offset / (total - visible)`.
 *
 * The widget is purely presentational. State lives in the calling app:
 *
 * {{{
 * given Theme = Theme.dark
 * ScrollBar(
 *   state = ScrollBar.State(offset = 12, visible = 20, total = 100),
 *   length = 20,
 *   at = Coord(80.x, 1.y)
 * )
 * }}}
 *
 * For mouse interaction the companion [[hitTest]] maps a click on the
 * track to a target offset (page-up / page-down / direct seek). Apps
 * pair that with [[clampOffset]] when handling drag gestures.
 *
 * The bar is hidden — emitted as no nodes — when `total <= visible`,
 * mirroring the typical "no scrollbar when everything fits" convention.
 */
object ScrollBar:

  /** Direction of the scrollbar's track. */
  enum Direction:
    case Vertical, Horizontal

  /**
   * Scroll position the bar visualises.
   *
   * @param offset  Index of the first visible item. Clamped to
   *                `[0, max(0, total - visible)]` at render time.
   * @param visible Number of items currently in view.
   * @param total   Total number of items in the underlying collection.
   */
  final case class State(offset: Int, visible: Int, total: Int):

    /** True when the bar should be drawn (more items than fit). */
    def needed: Boolean = total > visible && visible > 0

    /** Clamp `offset` to a valid range given `visible` and `total`. */
    def clamped: State =
      val maxOffset = math.max(0, total - visible)
      val cl        = math.max(0, math.min(maxOffset, offset))
      if cl == offset then this else copy(offset = cl)

  /** Default thumb glyph (vertical track). */
  val verticalThumb: Char = '█'

  /** Default thumb glyph (horizontal track). */
  val horizontalThumb: Char = '█'

  /** Default track glyph (vertical track). */
  val verticalTrack: Char = '│'

  /** Default track glyph (horizontal track). */
  val horizontalTrack: Char = '─'

  /**
   * Render a scrollbar. `length` is the number of cells along the
   * primary axis (rows for vertical, cols for horizontal). Returns the
   * empty list when no bar is needed (`total <= visible`).
   */
  def apply(
    state: State,
    length: Int,
    direction: Direction = Direction.Vertical,
    at: Coord = Coord(XCoord(1), YCoord(1))
  )(using theme: Theme): List[VNode] =
    if length <= 0 || !state.needed then Nil
    else
      val (thumbStart, thumbLen) = thumbRange(state, length)
      val trackStyle             = Style(fg = theme.secondary)
      val thumbStyle             = Style(fg = theme.primary, bold = true)
      direction match
        case Direction.Vertical =>
          (0 until length).toList.map { i =>
            val ch =
              if i >= thumbStart && i < thumbStart + thumbLen then verticalThumb
              else verticalTrack
            val style = if i >= thumbStart && i < thumbStart + thumbLen then thumbStyle else trackStyle
            TextNode(at.x, at.y + i, List(Text(ch.toString, style)))
          }
        case Direction.Horizontal =>
          val buf = Array.fill(length)(horizontalTrack)
          var i   = thumbStart
          while i < thumbStart + thumbLen && i < length do
            buf(i) = horizontalThumb
            i += 1
          // Two segments: track-thumb-track is fine in a single TextNode if we
          // split the run into styled chunks.
          val segments = chunkByThumb(buf.mkString, thumbStart, thumbLen, trackStyle, thumbStyle)
          List(TextNode(at.x, at.y, segments))

  /**
   * Compute `(startOffset, lengthInCells)` of the thumb along the bar.
   *
   * Both values are 0-based cell indices into the bar of `length` cells.
   * The thumb is at least one cell long (so it's visible even on a
   * massive backing collection) and is clamped to fit inside the track.
   */
  def thumbRange(state: State, length: Int): (Int, Int) =
    if length <= 0 || !state.needed then (0, 0)
    else
      val s         = state.clamped
      val visible   = math.max(1, s.visible)
      val total     = math.max(visible + 1, s.total)
      val thumbLen0 = (length.toDouble * visible / total).round.toInt
      val thumbLen  = math.max(1, math.min(length, thumbLen0))
      val maxStart  = length - thumbLen
      val maxOffset = math.max(1, total - visible)
      val start0    = (s.offset.toDouble / maxOffset * maxStart).round.toInt
      val start     = math.max(0, math.min(maxStart, start0))
      (start, thumbLen)

  /**
   * Map a click on the bar (cell index 0..length-1) to the target
   * `offset` the app should snap to.
   *
   * Strategy mirrors typical desktop scrollbars:
   *   - Click on the thumb itself returns the unchanged offset (the app
   *     should treat that as the start of a drag gesture).
   *   - Click before the thumb returns `offset - visible` (page up).
   *   - Click after the thumb returns `offset + visible` (page down).
   *
   * Result is clamped to `[0, total - visible]`.
   */
  def hitTest(state: State, length: Int, cellIndex: Int): Int =
    if !state.needed || length <= 0 then state.clamped.offset
    else
      val (thumbStart, thumbLen) = thumbRange(state, length)
      val cl                     = state.clamped
      val raw =
        if cellIndex < thumbStart then cl.offset - cl.visible
        else if cellIndex >= thumbStart + thumbLen then cl.offset + cl.visible
        else cl.offset
      math.max(0, math.min(math.max(0, cl.total - cl.visible), raw))

  /**
   * Compute the offset implied by a "drag the thumb to cell N" gesture.
   *
   * Linear mapping: `offset = (cellIndex / (length - thumbLen)) *
   * (total - visible)`. Clamped to `[0, total - visible]`.
   */
  def offsetForDrag(state: State, length: Int, cellIndex: Int): Int =
    if !state.needed || length <= 0 then state.clamped.offset
    else
      val (_, thumbLen) = thumbRange(state, length)
      val maxStart      = math.max(1, length - thumbLen)
      val cl            = state.clamped
      val maxOffset     = math.max(0, cl.total - cl.visible)
      val raw           = (cellIndex.toDouble / maxStart * maxOffset).round.toInt
      math.max(0, math.min(maxOffset, raw))

  /** Clamp an offset to the valid scroll range for `visible` / `total`. */
  def clampOffset(offset: Int, visible: Int, total: Int): Int =
    val maxOffset = math.max(0, total - visible)
    math.max(0, math.min(maxOffset, offset))

  private def chunkByThumb(
    line: String,
    thumbStart: Int,
    thumbLen: Int,
    trackStyle: Style,
    thumbStyle: Style
  ): List[Text] =
    val segs = List.newBuilder[Text]
    if thumbStart > 0 then segs += Text(line.substring(0, thumbStart), trackStyle)
    if thumbLen > 0 then
      segs += Text(line.substring(thumbStart, math.min(line.length, thumbStart + thumbLen)), thumbStyle)
    val tail = math.min(line.length, thumbStart + thumbLen)
    if tail < line.length then segs += Text(line.substring(tail), trackStyle)
    segs.result()
