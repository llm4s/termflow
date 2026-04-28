package termflow.tui

object KeyDecoder:

  /**
   * Modifier-key state attached to an [[InputKey]] via [[InputKey.Modified]].
   *
   * Decoded from the xterm modifier byte that appears in CSI sequences such
   * as `ESC [ 1 ; 5 A` (Ctrl+ArrowUp) and `ESC [ 15 ; 2 ~` (Shift+F5).
   * The xterm encoding is `code = 1 + Shift + 2*Alt + 4*Ctrl + 8*Meta`.
   *
   * The `none` value (no modifiers) never appears wrapped in `Modified` —
   * the parser unwraps it to the bare key, so a downstream `case ArrowUp =>`
   * still matches when no modifier is held.
   *
   * @param shift Shift key was held.
   * @param alt   Alt / Option key was held.
   * @param ctrl  Control key was held.
   * @param meta  Meta / Super key was held (uncommon — most terminals do not emit this).
   */
  final case class Modifiers(
    shift: Boolean = false,
    alt: Boolean = false,
    ctrl: Boolean = false,
    meta: Boolean = false
  ):
    def isEmpty: Boolean  = !shift && !alt && !ctrl && !meta
    def nonEmpty: Boolean = !isEmpty

  object Modifiers:

    /** No modifiers held. */
    val none: Modifiers = Modifiers()

    /**
     * Decode the xterm modifier byte (parameter that follows the `;` in
     * sequences like `CSI 1 ; 5 A`). The valid range is 2..16; anything
     * outside is decoded as `none`.
     *
     * Encoding (xterm CTLSEQS): `code = 1 + Shift + 2*Alt + 4*Ctrl + 8*Meta`,
     * so `2` is Shift, `3` is Alt, `5` is Ctrl, `6` is Shift+Ctrl, …
     */
    def fromXtermCode(code: Int): Modifiers =
      if code < 2 || code > 16 then none
      else
        val n = code - 1
        Modifiers(
          shift = (n & 1) != 0,
          alt = (n & 2) != 0,
          ctrl = (n & 4) != 0,
          meta = (n & 8) != 0
        )

  /** Normalised key representation used by the TUI. */
  enum InputKey:
    case CharKey(ch: Char)
    case Ctrl(ch: Char)
    case Backspace
    case Delete
    case Insert
    case Home
    case End
    case PageUp
    case PageDown
    case Enter
    case Escape
    case ArrowUp
    case ArrowDown
    case ArrowLeft
    case ArrowRight

    /** Shift+Tab — produced by the `\u001b[Z` escape sequence on most terminals. */
    case BackTab
    case F1
    case F2
    case F3
    case F4
    case F5
    case F6
    case F7
    case F8
    case F9
    case F10
    case F11
    case F12

    /**
     * A base key with one or more [[Modifiers]] held. Produced by the CSI
     * parser when an `ESC [ 1 ; <mod> <letter>` or `ESC [ <key> ; <mod> ~`
     * sequence is recognised. The wrapped `key` is never itself a
     * `Modified`, and `mods.nonEmpty` is always true — bare keys are
     * returned without wrapping.
     */
    case Modified(key: InputKey, mods: Modifiers)

    /**
     * A run of pasted text delivered as a single key event by the xterm
     * bracketed-paste protocol (`ESC [ 200 ~ ... ESC [ 201 ~`), activated by
     * [[ANSI.enableBracketedPaste]] at startup. The `text` payload preserves
     * embedded newlines; subscribers should treat it as opaque input rather
     * than a stream of individual keypresses.
     */
    case Paste(text: String)

    /**
     * A pointing-device event decoded from the xterm SGR-1006 mouse
     * protocol. Multiplexed onto the key stream so apps can handle keyboard
     * and mouse uniformly without owning two separate sources of bytes from
     * the terminal — pattern-match on `Mouse(_)` in the same handler that
     * sees `CharKey('q')`.
     *
     * Mouse reporting is enabled by [[ANSI.enableMouse]] at startup when
     * [[Capabilities.mouse]] is true.
     */
    case Mouse(event: MouseEvent)

    /**
     * Internal sentinel used by parsers to consume a byte sequence without
     * surfacing an event to the application. Filtered by the decoder
     * thread before it reaches `InputRead.Key`, so apps never see this
     * value in practice — a guard is still worth having for tests that
     * call decode helpers directly.
     *
     * Today's only emitter: scroll-wheel `release` bytes (`CSI <64;c;r m`)
     * that some terminals send as a duplicate of the press. See
     * [[MouseEvent.fromSgr]].
     */
    case NoOp
    case Unknown(seq: String)
    case EndOfInput

  import InputKey._

  def decode(key: Int): InputKey =
    key match
      // Both CR (13, what most terminals send for Enter in raw mode where
      // CR-to-LF translation is disabled) and LF (10) decode as Enter.
      // Ctrl+M produces byte 13 too — at this layer we cannot distinguish
      // it from Enter, so we standardise on the higher-level intent.
      case 10 | 13 => Enter
      case 127     => Backspace
      case code if code >= 1 && code <= 26 =>
        Ctrl(('A' + code - 1).toChar)
      case code if code >= 32 && code <= 126 =>
        CharKey(code.toChar)
      case _ =>
        Unknown(key.toString)

object AsciiControl:
  val ESC     = 27
  val O       = 79
  val `[`     = 91
  val P       = 80
  val Q       = 81
  val R       = 82
  val S       = 83
  val A       = 65
  val B       = 66
  val C       = 67
  val D       = 68
  val F       = 70
  val H       = 72
  val Z       = 90
  val `48`    = 48
  val `49`    = 49
  val `53`    = 53
  val `55`    = 55
  val `56`    = 56
  val `50`    = 50
  val `51`    = 51
  val `52`    = 52
  val `57`    = 57
  val `~`     = 126
  val `CTR+C` = 3
  val `CTR+D` = 4
