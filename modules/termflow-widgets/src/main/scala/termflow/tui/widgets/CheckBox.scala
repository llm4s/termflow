package termflow.tui.widgets

import termflow.tui.*

/**
 * Inline-styled checkbox rendered as `☒ Label` (checked) or `☐ Label`
 * (unchecked). When the active terminal does not advertise UTF-8 the
 * widget falls back to ASCII `[x]` / `[ ]` so it stays readable on
 * dumb terminals.
 *
 * `focused` and `checked` are independent — focus is communicated via
 * colour and boldness, the check state via the marker glyph. The shape
 * never changes on focus, so siblings don't have to reflow.
 *
 * Natural width is `markerWidth + 1 + label.length`. Height is always 1.
 *
 * Apps drive the toggle from their own keymap (typically Space or Enter
 * on the focused checkbox); this helper only renders the visual.
 *
 * {{{
 * given Theme = Theme.dark
 * Layout.column(gap = 0)(
 *   CheckBox("Enable autosave", checked = true,  focused = false),
 *   CheckBox("Show line numbers", checked = false, focused = true)
 * )
 * }}}
 *
 * @param label    Text rendered to the right of the marker.
 * @param checked  Whether the checkbox is in the "on" state.
 * @param focused  Whether the checkbox owns focus.
 * @param at       Top-left cell. Defaults to `(1, 1)` for layout use.
 * @param unicode  Whether to emit Unicode glyphs (default true) or ASCII
 *                 fallback. Apps that read this from the active
 *                 [[Capabilities.unicode]] flag can pass it through
 *                 explicitly.
 */
object CheckBox:

  /** Unicode marker glyphs — used when `unicode = true`. */
  private val checkedGlyph   = "☒"
  private val uncheckedGlyph = "☐"

  /** ASCII fallback glyphs — used when `unicode = false`. */
  private val checkedAscii   = "[x]"
  private val uncheckedAscii = "[ ]"

  def apply(
    label: String,
    checked: Boolean,
    focused: Boolean = false,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    unicode: Boolean = true
  )(using theme: Theme): VNode =
    val fg     = if focused then theme.primary else theme.foreground
    val marker = glyph(checked, unicode)
    TextNode(
      at.x,
      at.y,
      List(
        Text(marker, Style(fg = if checked then theme.success else fg, bold = focused)),
        Text(s" $label", Style(fg = fg, bold = focused))
      )
    )

  /** Marker glyph for a (checked, unicode) combination. */
  def glyph(checked: Boolean, unicode: Boolean = true): String =
    (checked, unicode) match
      case (true, true)   => checkedGlyph
      case (false, true)  => uncheckedGlyph
      case (true, false)  => checkedAscii
      case (false, false) => uncheckedAscii

  /** Natural cell width of a checkbox rendering `label`. */
  def width(label: String, unicode: Boolean = true): Int =
    glyph(checked = true, unicode).length + 1 + label.length
