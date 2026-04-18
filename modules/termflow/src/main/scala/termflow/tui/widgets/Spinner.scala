package termflow.tui.widgets

import termflow.tui.*
import termflow.tui.TuiPrelude.*

/**
 * Indeterminate progress indicator.
 *
 * The app drives the animation by incrementing an integer frame counter and
 * passing it to the widget — typically from a `Sub.Every` tick. The widget
 * is purely presentational.
 *
 * {{{
 * given Theme = Theme.dark
 * Spinner(Spinner.Braille, frame = model.tick)
 * }}}
 *
 * Several frame sets are provided as constants on the companion:
 *
 *   - [[Spinner.Braille]] — smooth rotating braille dots
 *   - [[Spinner.Line]]    — classic ASCII `|/-\`
 *   - [[Spinner.Dots]]    — three-dot ellipsis walk
 *
 * @param frames Ordered frame strings. Must be non-empty; the widget cycles through them via `frame % frames.size`.
 * @param frame  Current animation tick. Any non-negative integer; taken modulo `frames.size`.
 * @param at     Top-left cell. Defaults to `(1, 1)` for layout composition.
 */
object Spinner:

  val Braille: Vector[String] = Vector("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")
  val Line: Vector[String]    = Vector("|", "/", "-", "\\")
  val Dots: Vector[String]    = Vector(".  ", ".. ", "...", " ..", "  .", "   ")

  def apply(
    frames: Vector[String],
    frame: Int,
    at: Coord = Coord(XCoord(1), YCoord(1))
  )(using theme: Theme): VNode =
    require(frames.nonEmpty, "Spinner.frames must be non-empty")
    val idx = ((frame % frames.size) + frames.size) % frames.size
    val ch  = frames(idx)
    TextNode(at.x, at.y, List(ch.text(fg = theme.primary)))

  /** Width of the current frame in a spinner set. Frames may be multi-cell (e.g. [[Dots]]). */
  def width(frames: Vector[String]): Int =
    if frames.isEmpty then 0 else frames.map(_.length).max
