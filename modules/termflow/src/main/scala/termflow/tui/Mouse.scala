package termflow.tui

/** Pointing-device button reported by the SGR mouse protocol. */
enum MouseButton:
  case Left
  case Middle
  case Right

  /** A reported xterm "extra" button (back / forward on five-button mice). */
  case Other(code: Int)

/** Direction reported by [[MouseEvent.Scroll]]. */
enum ScrollDirection:
  case Up
  case Down
  case Left
  case Right

/**
 * A single mouse event decoded from the xterm SGR-1006 mouse protocol
 * (`CSI < button ; col ; row M / m`).
 *
 * Coordinates are 1-based to match every other coordinate pair in the
 * library — `col=1, row=1` is the top-left cell.
 *
 * The capture mode (xterm "any-event tracking", `CSI ?1003h`) decides
 * whether [[Move]] events without a held button arrive at all; today the
 * runtime enables button-event tracking only (`CSI ?1002h`), so apps will
 * see [[Press]] / [[Release]] / [[Drag]] / [[Scroll]] but no bare moves.
 * The enum is wired up regardless so a future "track everything" toggle
 * doesn't change the public API.
 */
enum MouseEvent:
  case Press(button: MouseButton, col: Int, row: Int, mods: KeyDecoder.Modifiers)
  case Release(button: MouseButton, col: Int, row: Int, mods: KeyDecoder.Modifiers)
  case Drag(button: MouseButton, col: Int, row: Int, mods: KeyDecoder.Modifiers)
  case Move(col: Int, row: Int, mods: KeyDecoder.Modifiers)
  case Scroll(direction: ScrollDirection, col: Int, row: Int, mods: KeyDecoder.Modifiers)

  /** Position the event references on the terminal grid (1-based). */
  def at: (Int, Int) = this match
    case Press(_, c, r, _)   => (c, r)
    case Release(_, c, r, _) => (c, r)
    case Drag(_, c, r, _)    => (c, r)
    case Move(c, r, _)       => (c, r)
    case Scroll(_, c, r, _)  => (c, r)

  /** Modifier-key state at the time of the event. */
  def modifiers: KeyDecoder.Modifiers = this match
    case Press(_, _, _, m)   => m
    case Release(_, _, _, m) => m
    case Drag(_, _, _, m)    => m
    case Move(_, _, m)       => m
    case Scroll(_, _, _, m)  => m

object MouseEvent:

  /**
   * Decode an SGR mouse sequence.
   *
   * @param button       Raw button byte from the SGR sequence. Bits 0..1
   *                     pick the base button (or `3` for "no button" on
   *                     motion); bit 2 = shift, bit 3 = alt, bit 4 = ctrl,
   *                     bit 5 = motion (drag / move), bit 6 = scroll, bit 7
   *                     = extra (button 8/9 etc).
   * @param col          1-based column.
   * @param row          1-based row.
   * @param releaseFinal True when the final byte was `m` (release); false
   *                     when it was `M` (press / motion / scroll).
   */
  def fromSgr(button: Int, col: Int, row: Int, releaseFinal: Boolean): Option[MouseEvent] =
    val mods = KeyDecoder.Modifiers(
      shift = (button & 0x04) != 0,
      alt = (button & 0x08) != 0,
      ctrl = (button & 0x10) != 0
    )
    val isScroll = (button & 0x40) != 0
    val isMotion = (button & 0x20) != 0
    val baseCode = button & 0x03
    val extraCode =
      if (button & 0x80) != 0 then Some(8 + (button & 0x03))
      else if isScroll then Some(button & 0x43) // keep the scroll bits intact for downstream
      else None

    if isScroll then
      // Some terminals (gnome-terminal, modern xterms) emit BOTH `M` (press)
      // and `m` (release) for a single wheel tick. xterm CTLSEQS only
      // mandates the press form. We treat the release as a duplicate and
      // drop it so apps see one Scroll event per physical wheel detent.
      if releaseFinal then None
      else
        val dir = button & 0x03 match
          case 0 => Some(ScrollDirection.Up)
          case 1 => Some(ScrollDirection.Down)
          case 2 => Some(ScrollDirection.Left)
          case 3 => Some(ScrollDirection.Right)
          case _ => None
        dir.map(d => Scroll(d, col, row, mods))
    else
      val btn = (button & 0x80) match
        case 0 =>
          baseCode match
            case 0 => MouseButton.Left
            case 1 => MouseButton.Middle
            case 2 => MouseButton.Right
            case _ => MouseButton.Other(baseCode)
        case _ => MouseButton.Other(extraCode.getOrElse(baseCode))

      if isMotion then
        // Motion with the no-button code (3) is a bare Move; otherwise it's
        // a Drag with the held button.
        if baseCode == 3 then Some(Move(col, row, mods))
        else Some(Drag(btn, col, row, mods))
      else if releaseFinal then Some(Release(btn, col, row, mods))
      else Some(Press(btn, col, row, mods))
