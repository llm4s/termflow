package termflow.tui.widgets

import termflow.tui.*
import termflow.tui.TuiPrelude.*

/**
 * Inline-styled button rendered as `[ Label ]`.
 *
 * Focus is communicated through colour and boldness: an unfocused button uses
 * the theme's `foreground` slot, a focused button uses `primary` and bolds
 * the label. The shape of the widget doesn't change on focus, so siblings
 * don't have to reflow when focus moves.
 *
 * Natural width is `label.length + 4`. Height is always `1`.
 *
 * {{{
 * given Theme = Theme.dark
 * Layout.row(gap = 2)(
 *   Button(label = "Save",   focused = true),
 *   Button(label = "Cancel", focused = false)
 * )
 * }}}
 *
 * The returned node is authored at `(1.x, 1.y)` so it composes cleanly with
 * [[Layout]]. To place it directly inside a `RootNode`, pass an explicit
 * `at` coordinate or translate the result with `Layout.translate`.
 *
 * @param label Button label. Rendered as-is (no truncation).
 * @param focused Whether the button currently owns focus.
 * @param at Top-left cell. Defaults to `(1, 1)` for use inside a layout.
 */
object Button:
  def apply(
    label: String,
    focused: Boolean = false,
    at: Coord = Coord(XCoord(1), YCoord(1))
  )(using theme: Theme): VNode =
    val fg = if focused then theme.primary else theme.foreground
    TextNode(
      at.x,
      at.y,
      List(
        "[ ".text(fg = fg),
        Text(label, Style(fg = fg, bold = focused)),
        " ]".text(fg = fg)
      )
    )

  /** Natural width of a button rendering `label`. */
  def width(label: String): Int = label.length + 4
