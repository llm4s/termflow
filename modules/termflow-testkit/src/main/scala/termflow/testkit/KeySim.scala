package termflow.testkit

import termflow.tui.KeyDecoder.InputKey
import termflow.tui.KeyDecoder.Modifiers

/**
 * Factory helpers for synthesising [[InputKey]] events in tests.
 *
 * `KeySim` exists so test bodies don't have to spell out the full
 * `KeyDecoder.InputKey.CharKey('h')` / `Modified(ArrowUp, Modifiers(...))`
 * incantations every time. The helpers return plain `InputKey` values, so
 * apps wrap them in their own `Msg.Key(...)` (or whatever name they use) and
 * feed through [[TuiTestDriver.send]].
 *
 * {{{
 * import termflow.testkit.KeySim
 *
 * KeySim.typeString("hello\n").foreach(k => driver.send(Msg.Key(k)))
 * driver.send(Msg.Key(KeySim.ctrlShift(KeySim.ArrowLeft)))
 * driver.send(Msg.Key(KeySim.f(5)))
 * }}}
 *
 * Pairs with [[MouseSim]], which produces `InputKey.Mouse(...)` values for
 * the same input pipeline.
 */
object KeySim:

  // ---- Named keys ---------------------------------------------------------

  /**
   * Re-export of the literal `InputKey` enum cases as fields so callers
   * can write `KeySim.Enter` without importing `KeyDecoder.InputKey.*`.
   */
  val Enter: InputKey      = InputKey.Enter
  val Escape: InputKey     = InputKey.Escape
  val Backspace: InputKey  = InputKey.Backspace
  val Delete: InputKey     = InputKey.Delete
  val Insert: InputKey     = InputKey.Insert
  val Home: InputKey       = InputKey.Home
  val End: InputKey        = InputKey.End
  val PageUp: InputKey     = InputKey.PageUp
  val PageDown: InputKey   = InputKey.PageDown
  val ArrowUp: InputKey    = InputKey.ArrowUp
  val ArrowDown: InputKey  = InputKey.ArrowDown
  val ArrowLeft: InputKey  = InputKey.ArrowLeft
  val ArrowRight: InputKey = InputKey.ArrowRight
  val BackTab: InputKey    = InputKey.BackTab
  val Tab: InputKey        = InputKey.CharKey('\t')

  // ---- Character + control ------------------------------------------------

  /** Plain printable character keystroke. */
  def char(c: Char): InputKey = InputKey.CharKey(c)

  /** Ctrl-modified character (e.g. `Ctrl+C` is `ctrl('C')`). */
  def ctrl(c: Char): InputKey = InputKey.Ctrl(c)

  /** Function key by 1-based index in `[1, 12]`. */
  def f(n: Int): InputKey = n match
    case 1  => InputKey.F1
    case 2  => InputKey.F2
    case 3  => InputKey.F3
    case 4  => InputKey.F4
    case 5  => InputKey.F5
    case 6  => InputKey.F6
    case 7  => InputKey.F7
    case 8  => InputKey.F8
    case 9  => InputKey.F9
    case 10 => InputKey.F10
    case 11 => InputKey.F11
    case 12 => InputKey.F12
    case _ =>
      throw new IllegalArgumentException(s"KeySim.f only supports F1..F12; got F$n")

  /** Bracketed-paste payload — a single event carrying the whole pasted text. */
  def paste(text: String): InputKey = InputKey.Paste(text)

  // ---- Modifiers ----------------------------------------------------------

  /**
   * Wrap `key` in [[InputKey.Modified]] with the given modifier flags.
   *
   * Bare keys (no modifiers) are returned unchanged so the same convention
   * the parser uses — never wrap an unmodified key — is preserved. Wrapping
   * an already-modified key merges the new flags into the existing set.
   */
  def modified(
    key: InputKey,
    shift: Boolean = false,
    alt: Boolean = false,
    ctrl: Boolean = false,
    meta: Boolean = false
  ): InputKey =
    val incoming = Modifiers(shift = shift, alt = alt, ctrl = ctrl, meta = meta)
    if incoming.isEmpty then key
    else
      key match
        case InputKey.Modified(inner, existing) =>
          InputKey.Modified(
            inner,
            Modifiers(
              shift = existing.shift || incoming.shift,
              alt = existing.alt || incoming.alt,
              ctrl = existing.ctrl || incoming.ctrl,
              meta = existing.meta || incoming.meta
            )
          )
        case other => InputKey.Modified(other, incoming)

  /** Shorthand: shift-modify a key. */
  def shift(key: InputKey): InputKey = modified(key, shift = true)

  /** Shorthand: alt-modify a key. */
  def alt(key: InputKey): InputKey = modified(key, alt = true)

  /** Shorthand: ctrl-modify a key. Use [[ctrl(Char)]] for control characters. */
  def ctrlMod(key: InputKey): InputKey = modified(key, ctrl = true)

  /** Shorthand: meta-modify a key. */
  def meta(key: InputKey): InputKey = modified(key, meta = true)

  /** Shorthand: shift+ctrl-modify a key. */
  def ctrlShift(key: InputKey): InputKey = modified(key, ctrl = true, shift = true)

  /** Shorthand: shift+alt-modify a key. */
  def altShift(key: InputKey): InputKey = modified(key, alt = true, shift = true)

  // ---- Strings ------------------------------------------------------------

  /**
   * Convert a string into a sequence of keystrokes, one [[InputKey]] per
   * character.
   *
   * Newlines (`\n`, `\r\n`, `\r`) become [[InputKey.Enter]] events;
   * everything else — including `\t` — becomes [[InputKey.CharKey]]. To
   * emulate a paste (one event for the whole payload), use [[paste]]
   * instead.
   *
   * Returns a `Vector` so callers can `foreach` or splice without
   * exhausting an iterator.
   */
  def typeString(s: String): Vector[InputKey] =
    val builder = Vector.newBuilder[InputKey]
    var i       = 0
    while i < s.length do
      val ch = s.charAt(i)
      ch match
        case '\r' =>
          builder += InputKey.Enter
          // Consume the paired LF in CRLF so we don't emit two Enters.
          if i + 1 < s.length && s.charAt(i + 1) == '\n' then i += 1
        case '\n' =>
          builder += InputKey.Enter
        case other =>
          builder += InputKey.CharKey(other)
      i += 1
    builder.result()
