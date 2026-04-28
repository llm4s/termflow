package termflow.tui.widgets

import termflow.tui.*

/**
 * Single-cell-thick line used to visually separate two regions.
 *
 * Renders a horizontal or vertical run of the active theme's
 * [[BorderChars]] glyphs (defaulting to the same style used by box
 * borders). Optionally embeds a centred title in the run, in which case
 * the line is broken with a small leader / trailer:
 *
 * {{{
 * ──── Title ────
 * }}}
 *
 * The widget is purely presentational — it owns no state and emits a
 * single [[TextNode]]. Apps typically place it between rows in a
 * `Layout.column` or between columns in a `Layout.row`:
 *
 * {{{
 * given Theme = Theme.dark
 * Layout.column(gap = 0)(
 *   Header(...),
 *   Separator.horizontal(width = 80),
 *   Body(...)
 * )
 * }}}
 *
 * Alignment within a multi-cell row / column is the caller's job; the
 * separator just draws the line at the supplied `at`.
 */
object Separator:

  /** Direction of the separator. */
  enum Direction:
    case Horizontal, Vertical

  /**
   * Horizontal separator of `width` cells.
   *
   * @param width  Length of the run in cells. `width <= 0` yields no node
   *               (returns an empty list), which keeps `Layout.column`
   *               composition simple.
   * @param at     Top-left cell. Defaults to `(1, 1)`.
   * @param title  Optional centred title. Rendered with the theme's
   *               primary slot; the surrounding rule uses the theme's
   *               border slot. If `width` cannot fit the title plus
   *               at least one cell of leader / trailer the title is
   *               dropped and the rule fills the row.
   * @param chars  Glyph set to draw with. Defaults to the theme's chars.
   */
  def horizontal(
    width: Int,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    title: String = "",
    chars: BorderChars = null
  )(using theme: Theme): List[VNode] =
    if width <= 0 then Nil
    else
      val activeChars = if chars eq null then theme.chars else chars
      val rule        = activeChars.horizontal.toString
      val ruleStyle   = Style(fg = theme.foreground)
      val titleStyle  = Style(fg = theme.primary, bold = true)
      val titleText   = if title.isEmpty then "" else s" $title "
      // Need at least one cell of leader and one of trailer around the title.
      if titleText.isEmpty || titleText.length + 2 > width then
        List(TextNode(at.x, at.y, List(Text(rule * width, ruleStyle))))
      else
        val leader  = (width - titleText.length) / 2
        val trailer = width - titleText.length - leader
        List(
          TextNode(
            at.x,
            at.y,
            List(
              Text(rule * leader, ruleStyle),
              Text(titleText, titleStyle),
              Text(rule * trailer, ruleStyle)
            )
          )
        )

  /**
   * Vertical separator of `height` cells.
   *
   * Emits one single-character `TextNode` per row — the simplest model
   * given the renderer's row-per-text-node convention. Like
   * [[horizontal]], a non-positive `height` yields no nodes.
   *
   * @param height Number of cells in the run.
   * @param at     Top-left cell. Defaults to `(1, 1)`.
   * @param chars  Glyph set to draw with. Defaults to the theme's chars.
   */
  def vertical(
    height: Int,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    chars: BorderChars = null
  )(using theme: Theme): List[VNode] =
    if height <= 0 then Nil
    else
      val activeChars = if chars eq null then theme.chars else chars
      val style       = Style(fg = theme.foreground)
      val glyph       = activeChars.vertical.toString
      (0 until height).map(i => TextNode(at.x, at.y + i, List(Text(glyph, style)))).toList
