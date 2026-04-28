package termflow.testkit

import termflow.tui.KeyDecoder.InputKey
import termflow.tui.KeyDecoder.Modifiers
import termflow.tui.MouseButton
import termflow.tui.MouseEvent
import termflow.tui.ScrollDirection

/**
 * Factory helpers for synthesising mouse events in tests.
 *
 * Mouse events are multiplexed onto the key stream as
 * `InputKey.Mouse(MouseEvent)` (Stage 2 §5.1), so every helper here returns
 * an [[InputKey]] ready to drop into the same `Msg.Key(...)` handler that
 * receives keyboard events.
 *
 * {{{
 * import termflow.testkit.MouseSim
 *
 * driver.send(Msg.Key(MouseSim.click(col = 7, row = 3)))
 * MouseSim.scrollDown(col = 10, row = 10, ticks = 3)
 *   .foreach(k => driver.send(Msg.Key(k)))
 * }}}
 *
 * Coordinates follow the rest of the library: 1-based, with `(1, 1)` at the
 * top-left cell.
 */
object MouseSim:

  /** No modifiers — convenience alias for the common case. */
  val NoMods: Modifiers = Modifiers.none

  // ---- Single events ------------------------------------------------------

  /**
   * Synthesise a left-button click — a [[MouseEvent.Press]] / `InputKey.Mouse`
   * pair. Most apps consume `Press` directly; if your handler waits for
   * `Release`, use [[clickPair]] to get both events.
   */
  def click(col: Int, row: Int, button: MouseButton = MouseButton.Left, mods: Modifiers = NoMods): InputKey =
    press(col, row, button, mods)

  /** Press event for `button` at `(col, row)`. */
  def press(col: Int, row: Int, button: MouseButton = MouseButton.Left, mods: Modifiers = NoMods): InputKey =
    InputKey.Mouse(MouseEvent.Press(button, col, row, mods))

  /** Release event for `button` at `(col, row)`. */
  def release(col: Int, row: Int, button: MouseButton = MouseButton.Left, mods: Modifiers = NoMods): InputKey =
    InputKey.Mouse(MouseEvent.Release(button, col, row, mods))

  /** Drag event — motion with a held `button` at `(col, row)`. */
  def drag(col: Int, row: Int, button: MouseButton = MouseButton.Left, mods: Modifiers = NoMods): InputKey =
    InputKey.Mouse(MouseEvent.Drag(button, col, row, mods))

  /**
   * Bare motion (no held button) at `(col, row)`. The runtime only enables
   * any-event tracking on demand, so most apps never see these in
   * production — simulate them anyway when wiring hover behaviour.
   */
  def move(col: Int, row: Int, mods: Modifiers = NoMods): InputKey =
    InputKey.Mouse(MouseEvent.Move(col, row, mods))

  /** Single scroll-wheel detent in `direction`. Use [[scroll]] for bursts. */
  def scrollOnce(
    direction: ScrollDirection,
    col: Int,
    row: Int,
    mods: Modifiers = NoMods
  ): InputKey =
    InputKey.Mouse(MouseEvent.Scroll(direction, col, row, mods))

  // ---- Sequences ----------------------------------------------------------

  /**
   * Press + release pair for `button` at `(col, row)`. Useful for apps that
   * activate on `Release` rather than `Press`. The two events share
   * coordinates and modifier state — a real terminal can shift between
   * them but a synthetic test usually doesn't care.
   */
  def clickPair(
    col: Int,
    row: Int,
    button: MouseButton = MouseButton.Left,
    mods: Modifiers = NoMods
  ): Vector[InputKey] =
    Vector(
      press(col, row, button, mods),
      release(col, row, button, mods)
    )

  /**
   * `ticks` consecutive scroll detents in `direction`. Common shorthand
   * for "the user wheeled three notches down at (col, row)".
   *
   * @throws IllegalArgumentException if `ticks` is negative.
   */
  def scroll(
    direction: ScrollDirection,
    col: Int,
    row: Int,
    ticks: Int = 1,
    mods: Modifiers = NoMods
  ): Vector[InputKey] =
    require(ticks >= 0, s"MouseSim.scroll ticks must be non-negative, got $ticks")
    Vector.fill(ticks)(scrollOnce(direction, col, row, mods))

  /** Convenience: `ticks` scroll-up detents at `(col, row)`. */
  def scrollUp(col: Int, row: Int, ticks: Int = 1, mods: Modifiers = NoMods): Vector[InputKey] =
    scroll(ScrollDirection.Up, col, row, ticks, mods)

  /** Convenience: `ticks` scroll-down detents at `(col, row)`. */
  def scrollDown(col: Int, row: Int, ticks: Int = 1, mods: Modifiers = NoMods): Vector[InputKey] =
    scroll(ScrollDirection.Down, col, row, ticks, mods)

  /** Convenience: `ticks` scroll-left detents at `(col, row)`. */
  def scrollLeft(col: Int, row: Int, ticks: Int = 1, mods: Modifiers = NoMods): Vector[InputKey] =
    scroll(ScrollDirection.Left, col, row, ticks, mods)

  /** Convenience: `ticks` scroll-right detents at `(col, row)`. */
  def scrollRight(col: Int, row: Int, ticks: Int = 1, mods: Modifiers = NoMods): Vector[InputKey] =
    scroll(ScrollDirection.Right, col, row, ticks, mods)

  /**
   * A drag gesture from `(fromCol, fromRow)` to `(toCol, toRow)`: a press,
   * a series of [[MouseEvent.Drag]] events along a straight Bresenham-ish
   * path, and a final release. The path is sampled at `steps`+1 points so
   * tests with hit-testing can validate the intermediate positions —
   * `steps = 0` collapses to press + release at the start, then end.
   *
   * @param steps Number of intermediate drag samples between the endpoints
   *              (excluding press / release). Must be non-negative.
   */
  def dragGesture(
    fromCol: Int,
    fromRow: Int,
    toCol: Int,
    toRow: Int,
    button: MouseButton = MouseButton.Left,
    steps: Int = 4,
    mods: Modifiers = NoMods
  ): Vector[InputKey] =
    require(steps >= 0, s"MouseSim.dragGesture steps must be non-negative, got $steps")
    val builder = Vector.newBuilder[InputKey]
    builder += press(fromCol, fromRow, button, mods)
    if steps > 0 then
      val total = steps + 1
      var i     = 1
      while i <= steps do
        val t   = i.toDouble / total.toDouble
        val col = math.round(fromCol + (toCol - fromCol) * t).toInt
        val row = math.round(fromRow + (toRow - fromRow) * t).toInt
        builder += drag(col, row, button, mods)
        i += 1
    builder += drag(toCol, toRow, button, mods)
    builder += release(toCol, toRow, button, mods)
    builder.result()
