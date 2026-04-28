package termflow.tui.widgets

import termflow.tui.*

/**
 * Vertical single-select widget. Renders one row per option with a
 * `◉` (selected) or `○` (unselected) marker, falling back to ASCII
 * `(*)` / `( )` when `unicode = false`.
 *
 * `selectedIndex` is the model truth — exactly one row is "on" at a
 * time. `focusedIndex` is independent: it controls which row currently
 * owns keyboard focus and is rendered in the theme's primary slot. A
 * row can be focused without being selected (the user has moved the
 * highlight but not yet pressed Space/Enter to commit).
 *
 * Apps drive arrow-key navigation by updating `focusedIndex` and
 * commit a selection by setting `selectedIndex = focusedIndex` on
 * Space/Enter — that pattern keeps every state transition pure and
 * testable.
 *
 * The rendered group is `options.size` rows tall and as wide as the
 * longest `marker + space + label`. Returned as a list of `VNode`s so
 * it composes inside [[Layout.Column]] without an extra wrapper.
 *
 * {{{
 * given Theme = Theme.dark
 * Layout.column(gap = 0)(
 *   RadioGroup(
 *     options = Seq("Light", "Dark", "Mono"),
 *     selectedIndex = 1,
 *     focusedIndex  = 1
 *   ): _*
 * )
 * }}}
 *
 * @param options       Display strings for each option.
 * @param selectedIndex Index of the option that is currently selected.
 * @param focusedIndex  Index of the focused row. `-1` means no row has
 *                      focus (e.g. focus has moved away from the group).
 * @param at            Top-left cell of the first row.
 * @param unicode       Whether to emit Unicode marker glyphs (default
 *                      true) or ASCII `(*)` / `( )` fallback.
 */
object RadioGroup:

  private val selectedGlyph   = "◉"
  private val unselectedGlyph = "○"
  private val selectedAscii   = "(*)"
  private val unselectedAscii = "( )"

  def apply(
    options: Seq[String],
    selectedIndex: Int,
    focusedIndex: Int = -1,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    unicode: Boolean = true
  )(using theme: Theme): List[VNode] =
    options.zipWithIndex.toList.map { case (label, i) =>
      val selected = i == selectedIndex
      val focused  = i == focusedIndex
      val fg       = if focused then theme.primary else theme.foreground
      val marker   = glyph(selected, unicode)
      TextNode(
        at.x,
        at.y + i,
        List(
          Text(marker, Style(fg = if selected then theme.success else fg, bold = focused)),
          Text(s" $label", Style(fg = fg, bold = focused))
        )
      )
    }

  /** Marker glyph for a (selected, unicode) combination. */
  def glyph(selected: Boolean, unicode: Boolean = true): String =
    (selected, unicode) match
      case (true, true)   => selectedGlyph
      case (false, true)  => unselectedGlyph
      case (true, false)  => selectedAscii
      case (false, false) => unselectedAscii

  /** Natural cell width — wide enough to fit the longest `marker label`. */
  def width(options: Seq[String], unicode: Boolean = true): Int =
    if options.isEmpty then 0
    else
      val markerLen = glyph(selected = true, unicode).length
      markerLen + 1 + options.map(_.length).max

  /** Cell height — one row per option. */
  def height(options: Seq[String]): Int = options.size
