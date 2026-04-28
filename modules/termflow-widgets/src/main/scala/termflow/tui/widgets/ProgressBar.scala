package termflow.tui.widgets

import termflow.tui.*
import termflow.tui.TuiPrelude.*

/**
 * Horizontal determinate progress bar.
 *
 * Renders as a filled portion (`█`) in the theme's `primary` slot followed
 * by an empty tail (`░`) in `foreground`, optionally suffixed with a
 * right-aligned percentage.
 *
 * {{{
 * given Theme = Theme.dark
 * ProgressBar(value = 0.75, width = 20)
 * // ████████████████░░░░  75%
 * }}}
 *
 * @param value       Progress in `[0.0, 1.0]`. Values outside the range are clamped.
 * @param width       Number of cells used by the bar itself (before the optional percentage).
 * @param showPercent If `true`, append `"  NN%"`. Default `true`.
 * @param at          Top-left cell. Defaults to `(1, 1)` for layout composition.
 *
 * Height is always `1`. Natural width is `width + 5` when `showPercent`, else
 * `width`. Use [[ProgressBar.renderedWidth]] to size sibling layouts.
 */
object ProgressBar:

  private val FilledChar = '█'
  private val EmptyChar  = '░'

  def apply(
    value: Double,
    width: Int,
    showPercent: Boolean = true,
    at: Coord = Coord(XCoord(1), YCoord(1))
  )(using theme: Theme): VNode =
    val clamped    = math.max(0.0, math.min(1.0, value))
    val safeW      = math.max(0, width)
    val filled     = math.round(clamped * safeW).toInt
    val empty      = math.max(0, safeW - filled)
    val filledText = if filled > 0 then FilledChar.toString * filled else ""
    val emptyText  = if empty > 0 then EmptyChar.toString * empty else ""
    val percent    = f" ${clamped * 100}%3.0f%%"
    val segments = List(
      filledText.text(fg = theme.primary),
      emptyText.text(fg = theme.foreground)
    ) ++ (if showPercent then List(percent.text(fg = theme.foreground)) else Nil)
    TextNode(at.x, at.y, segments)

  /** Natural width of a progress bar with the given settings. */
  def renderedWidth(width: Int, showPercent: Boolean = true): Int =
    math.max(0, width) + (if showPercent then 5 else 0)
