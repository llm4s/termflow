package termflow.tui.widgets

import termflow.tui.*

/**
 * Horizontal tab bar rendered as a single row.
 *
 * Each tab gets a `[ Label ]` (active) or `  Label  ` (inactive) box;
 * the active tab uses the theme's `primary` slot bold, focused-but-
 * inactive tabs use `primary` not bold, plain inactive tabs use
 * `foreground`. The shape doesn't change on focus — tabs always render
 * with two cells of padding on each side — so siblings never reflow when
 * focus or active state moves.
 *
 * Active and focused are independent. `activeIndex` is the truth (which
 * tab's content is being shown); `focusedIndex` controls which header
 * cell currently owns keyboard focus. A tab can be focused without
 * being active — the user has moved the highlight but not yet pressed
 * Enter / clicked to commit.
 *
 * Apps drive arrow-left / right (or Tab) navigation by updating
 * `focusedIndex` and commit the tab switch by setting
 * `activeIndex = focusedIndex` on Enter / Space.
 *
 * {{{
 * given Theme = Theme.dark
 * Tabs(
 *   labels        = Seq("Home", "Work", "Notes"),
 *   activeIndex   = 1,
 *   focusedIndex  = 1
 * )
 * }}}
 *
 * @param labels       One label per tab, drawn left-to-right.
 * @param activeIndex  Index of the active tab (`-1` for none).
 * @param focusedIndex Index of the focused tab. `-1` means no header
 *                     row owns focus (focus has moved into the body).
 * @param at           Top-left cell of the tab row.
 * @param separator    String drawn between adjacent tab cells. Use
 *                     `" "` for a quiet bar, `" │ "` for a vertical
 *                     divider, `""` to render tabs flush against each
 *                     other.
 */
object Tabs:

  /** Width of the constant padding (`[` + space + label + space + `]`). */
  private val Padding: Int = 4

  def apply(
    labels: Seq[String],
    activeIndex: Int,
    focusedIndex: Int = -1,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    separator: String = " "
  )(using theme: Theme): VNode =
    val segments: List[Text] =
      labels.zipWithIndex.toList.flatMap { case (label, i) =>
        val isActive  = i == activeIndex
        val isFocused = i == focusedIndex
        val fg =
          if isActive then theme.primary
          else if isFocused then theme.primary
          else theme.foreground
        val bold   = isActive || isFocused
        val opener = if isActive then "[ " else "  "
        val closer = if isActive then " ]" else "  "
        val cell   = Text(s"$opener$label$closer", Style(fg = fg, bold = bold))
        val gap =
          if i == labels.size - 1 || separator.isEmpty then None
          else Some(Text(separator, Style(fg = theme.border)))
        cell :: gap.toList
      }
    TextNode(at.x, at.y, segments)

  /**
   * Cell width of a tab bar rendering `labels` with `separator` between
   * them. Each tab takes `label.length + 4` cells (the constant `[ ` /
   * `  ` padding); separators sit between tabs.
   */
  def width(labels: Seq[String], separator: String = " "): Int =
    if labels.isEmpty then 0
    else labels.map(_.length + Padding).sum + (labels.size - 1) * separator.length

  /**
   * Map an absolute click column to the tab index it landed on. Returns
   * `None` if the column is outside any tab cell (for example, on a
   * separator).
   *
   * Apps with mouse hit-testing pass the click column relative to the
   * same `at.x` they used to render the bar — i.e. `mouseCol - at.x.value`.
   *
   * @param colOffset 0-based offset into the tab row.
   */
  def hitTest(labels: Seq[String], separator: String = " ", colOffset: Int): Option[Int] =
    if colOffset < 0 then None
    else
      var i      = 0
      var cursor = 0
      while i < labels.size do
        val cellEnd = cursor + labels(i).length + Padding
        if colOffset >= cursor && colOffset < cellEnd then return Some(i)
        cursor = cellEnd + separator.length
        i += 1
      None
