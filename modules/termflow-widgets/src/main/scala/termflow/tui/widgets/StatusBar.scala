package termflow.tui.widgets

import termflow.tui.*

/**
 * Bottom-of-screen status bar with left / center / right sections.
 *
 * Produces a single-row `TextNode` exactly `width` cells wide. The three
 * sections are placed independently:
 *
 *   - left  — flush to the left edge
 *   - center — centered within the row
 *   - right — flush to the right edge
 *
 * If sections overlap because the row is too narrow, the renderer drops
 * sections in priority order `right → center → left`. An oversized `left`
 * is truncated to fit.
 *
 * The bar uses the theme's `primary` colour as its background and
 * `background` as the foreground by default — the classic "inverse-video
 * status line" look. Override the styling by constructing the `TextNode`
 * directly if you need something bespoke.
 *
 * {{{
 * given Theme = Theme.dark
 * StatusBar(
 *   left   = " ready",
 *   center = "editor.scala",
 *   right  = "L:42 ",
 *   width  = model.terminalWidth,
 *   at     = Coord(1.x, (model.terminalHeight).y)
 * )
 * }}}
 */
object StatusBar:

  def apply(
    left: String,
    center: String,
    right: String,
    width: Int,
    at: Coord = Coord(XCoord(1), YCoord(1))
  )(using theme: Theme): VNode =
    val row   = renderString(left, center, right, math.max(0, width))
    val style = Style(fg = theme.background, bg = theme.primary)
    TextNode(at.x, at.y, List(Text(row, style)))

  /**
   * Compose the three sections into a single padded string of length `width`.
   *
   * Exposed for tests and for callers that want to apply custom styling
   * rather than going through [[apply]].
   */
  def renderString(left: String, center: String, right: String, width: Int): String =
    if width <= 0 then ""
    else
      val buf = Array.fill(width)(' ')

      // Left section truncated to total width.
      val leftFit = if left.length > width then left.take(width) else left
      var i       = 0
      while i < leftFit.length do
        buf(i) = leftFit.charAt(i)
        i += 1

      // Right section only if it doesn't overlap with left.
      val rightFit =
        if right.length > math.max(0, width - leftFit.length) then ""
        else right
      if rightFit.nonEmpty then
        val start = width - rightFit.length
        var j     = 0
        while j < rightFit.length do
          buf(start + j) = rightFit.charAt(j)
          j += 1

      // Center section only if it fits in the gap.
      val used      = leftFit.length + rightFit.length
      val available = math.max(0, width - used)
      val centerFit = if center.length > available then "" else center
      if centerFit.nonEmpty then
        val start = math.max(leftFit.length, (width - centerFit.length) / 2)
        val clampedStart =
          math.min(start, width - rightFit.length - centerFit.length)
        var k = 0
        while k < centerFit.length do
          buf(clampedStart + k) = centerFit.charAt(k)
          k += 1

      new String(buf)
