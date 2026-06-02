package termflow.tui

import termflow.tui.AsciiControl.*
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.KeyDecoder.Modifiers

import java.io.Reader
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.util.Success
import scala.util.Try

enum InputRead:
  case Key(key: InputKey)
  case End
  case Failed(cause: Throwable)

trait TerminalKeySource:
  def next(): InputRead
  def close(): Try[Unit]

object ConsoleKeyPressSource:
  private val EndOfStream                   = Int.MinValue
  private val csiPollTimeoutMs: Long        = 50L
  private val standaloneEscapeMs: Long      = 50L
  private val DefaultModifierIfMissing: Int = 1

  /**
   * Upper bound on the number of CSI parameter characters we buffer. A
   * well-behaved terminal never sends more than a handful; the cap stops a
   * malformed or hostile stream of digits from growing the buffer without
   * limit. Excess digits are dropped — combined with the overflow-safe parse
   * below, an over-long parameter degrades to `Unknown` rather than killing
   * the decoder thread.
   */
  private val MaxCsiParamChars: Int = 64

  /**
   * Top-level decoder: classifies a single byte as a printable, control,
   * standalone Escape, or the start of a CSI / SS3 sequence and dispatches
   * to the appropriate sub-parser.
   */
  private def processKey(c: Int, queueBridge: BlockingQueue[Integer]): InputKey =
    if c != ESC then KeyDecoder.decode(c)
    else
      // Standalone ESC: 50 ms grace period to see if a sequence follows.
      val next: Integer = queueBridge.poll(standaloneEscapeMs, TimeUnit.MILLISECONDS)
      Option(next).map(_.intValue()) match
        case None =>
          InputKey.Escape
        case Some(`EndOfStream`) =>
          queueBridge.put(EndOfStream)
          InputKey.Escape
        case Some(b) if b == `[` =>
          decodeCsi(queueBridge)
        case Some(b) if b == O =>
          decodeSs3(queueBridge)
        case Some(b) =>
          // ESC followed by any other byte is the conventional Alt+key
          // encoding (xterm "metaSendsEscape"). Wrap whatever the byte
          // decodes to with `alt = true`.
          val base = KeyDecoder.decode(b)
          withMods(base, Modifiers(alt = true))

  /**
   * Read a CSI body — parameter bytes (`0..9`, `;`) followed by a final
   * byte (any other printable) — then map to an `InputKey`. Returns
   * `Unknown` for malformed or unrecognised sequences.
   *
   * Bytes outside the CSI parameter alphabet that arrive before a final
   * byte (e.g. `?`, `<`, `>`, `!`) trigger a fallback that consumes until
   * the next final byte and reports `Unknown`. SGR mouse (`CSI < ...`)
   * will be handled separately at the mouse-support stage.
   */
  private def decodeCsi(queueBridge: BlockingQueue[Integer]): InputKey =
    val builder         = new StringBuilder
    var prefix: Char    = '\u0000'
    var finalByte: Char = '\u0000'
    var done            = false
    var endOfStream     = false

    while !done do
      val n: Integer = queueBridge.poll(csiPollTimeoutMs, TimeUnit.MILLISECONDS)
      if n == null then
        // Truncated CSI sequence — give up and report Unknown.
        finalByte = '\u0000'
        done = true
      else if n.intValue() == EndOfStream then
        queueBridge.put(EndOfStream)
        endOfStream = true
        done = true
      else
        val ch = n.intValue().toChar
        if (ch >= '0' && ch <= '9') || ch == ';' then
          // Drop overflow digits rather than buffering an unbounded run.
          if builder.length < MaxCsiParamChars then builder.append(ch): Unit
        else if prefix == '\u0000' && (ch == '?' || ch == '<' || ch == '>' || ch == '!') then prefix = ch
        else
          finalByte = ch
          done = true

    if endOfStream || finalByte == '\u0000' then InputKey.Unknown(s"CSI ${builder.toString}")
    else
      val params: Vector[Int] =
        if builder.isEmpty then Vector.empty
        // `toIntOption` guards against parameter values that overflow `Int`
        // (e.g. a malformed `CSI 99999999999999 A`). Before this guard the
        // resulting `NumberFormatException` propagated out of the decoder
        // thread, which then died and left keyboard/mouse input frozen for
        // the rest of the session. Unparseable / empty params fall back to 0.
        else builder.toString.split(";", -1).toVector.map(p => p.toIntOption.getOrElse(0))
      prefix match
        case '<'      => decodeSgrMouse(params, finalByte)
        case '\u0000' => csiToKey(params, finalByte, queueBridge)
        case other    => InputKey.Unknown(s"CSI $other${builder.toString}$finalByte")

  /**
   * SS3 prefix (`ESC O`): function-key encoding used by some terminals
   * for F1..F4, plus a few alternate Home/End forms.
   */
  private def decodeSs3(queueBridge: BlockingQueue[Integer]): InputKey =
    val n: Integer = queueBridge.poll(csiPollTimeoutMs, TimeUnit.MILLISECONDS)
    if n == null then InputKey.Escape
    else if n.intValue() == EndOfStream then
      queueBridge.put(EndOfStream)
      InputKey.Escape
    else
      n.intValue().toChar match
        case 'P'   => InputKey.F1
        case 'Q'   => InputKey.F2
        case 'R'   => InputKey.F3
        case 'S'   => InputKey.F4
        case 'H'   => InputKey.Home
        case 'F'   => InputKey.End
        case other => InputKey.Unknown(s"SS3 $other")

  /**
   * Map `(params, finalByte)` from a CSI body to an `InputKey`.
   *
   * Cursor / navigation keys use the `CSI 1 ; <mod> <letter>` form. The
   * leading `1` is the cursor-position marker; we accept it implicitly
   * and pull modifiers out of the second parameter.
   *
   * Function keys with parameters use `CSI <key> ; <mod> ~`; the modifier
   * (when present) is in the second parameter.
   */
  private def csiToKey(
    params: Vector[Int],
    finalByte: Char,
    queueBridge: BlockingQueue[Integer]
  ): InputKey =
    finalByte match
      case 'A' => withMods(InputKey.ArrowUp, modsAt(params, 1))
      case 'B' => withMods(InputKey.ArrowDown, modsAt(params, 1))
      case 'C' => withMods(InputKey.ArrowRight, modsAt(params, 1))
      case 'D' => withMods(InputKey.ArrowLeft, modsAt(params, 1))
      case 'H' => withMods(InputKey.Home, modsAt(params, 1))
      case 'F' => withMods(InputKey.End, modsAt(params, 1))
      case 'Z' => InputKey.BackTab
      case 'P' => withMods(InputKey.F1, modsAt(params, 1))
      case 'Q' => withMods(InputKey.F2, modsAt(params, 1))
      case 'R' => withMods(InputKey.F3, modsAt(params, 1))
      case 'S' => withMods(InputKey.F4, modsAt(params, 1))
      case '~' =>
        val keyId = params.headOption.getOrElse(DefaultModifierIfMissing)
        if keyId == 200 then collectPaste(queueBridge)
        else if keyId == 201 then
          // A stray paste-end marker without a matching start. Drop it
          // silently — surfacing it as Unknown would leak the protocol
          // marker to apps that didn't ask for it.
          InputKey.Unknown(s"CSI 201~ (orphan paste end)")
        else
          val mods = modsAt(params, 1)
          keyForTilde(keyId) match
            case Some(k) => withMods(k, mods)
            case None    => InputKey.Unknown(s"CSI ${params.mkString(";")}~")
      case _ =>
        InputKey.Unknown(s"CSI ${params.mkString(";")}$finalByte")

  /**
   * Map the leading parameter of a `CSI <n> ~` sequence to its base key.
   * Numbers come straight from xterm CTLSEQS:
   *   1, 7 → Home   2 → Insert   3 → Delete   4, 8 → End
   *   5 → PageUp    6 → PageDown
   *   11..15 → F1..F5   17..21 → F6..F10   23..24 → F11..F12
   */
  private def keyForTilde(n: Int): Option[InputKey] = n match
    case 1 | 7 => Some(InputKey.Home)
    case 2     => Some(InputKey.Insert)
    case 3     => Some(InputKey.Delete)
    case 4 | 8 => Some(InputKey.End)
    case 5     => Some(InputKey.PageUp)
    case 6     => Some(InputKey.PageDown)
    case 11    => Some(InputKey.F1)
    case 12    => Some(InputKey.F2)
    case 13    => Some(InputKey.F3)
    case 14    => Some(InputKey.F4)
    case 15    => Some(InputKey.F5)
    case 17    => Some(InputKey.F6)
    case 18    => Some(InputKey.F7)
    case 19    => Some(InputKey.F8)
    case 20    => Some(InputKey.F9)
    case 21    => Some(InputKey.F10)
    case 23    => Some(InputKey.F11)
    case 24    => Some(InputKey.F12)
    case _     => None

  private def modsAt(params: Vector[Int], idx: Int): Modifiers =
    if params.length > idx then Modifiers.fromXtermCode(params(idx)) else Modifiers.none

  private def withMods(key: InputKey, mods: Modifiers): InputKey =
    if mods.isEmpty then key else InputKey.Modified(key, mods)

  /**
   * Decode an SGR-1006 mouse sequence — `CSI < button ; col ; row M/m`.
   *
   * The terminal sends the final byte as `M` for press, motion, and scroll
   * events and `m` for releases; the button parameter is the encoded
   * button-and-modifier byte documented on [[MouseEvent.fromSgr]].
   */
  private def decodeSgrMouse(params: Vector[Int], finalByte: Char): InputKey =
    if params.length < 3 then InputKey.Unknown(s"CSI <${params.mkString(";")}$finalByte")
    else
      val button = params(0)
      // SGR mouse coordinates are 1-based; clamp to honour the invariant
      // documented on `MouseEvent` so downstream `col - 1` indexing can't
      // land at -1 if a terminal ever reports a 0.
      val col          = math.max(1, params(1))
      val row          = math.max(1, params(2))
      val releaseFinal = finalByte == 'm'
      // `fromSgr` returns None for the scroll-wheel release byte some
      // terminals duplicate after each press — surface as NoOp so the
      // decoder loop drops it instead of polluting the stream with
      // Unknowns. Other malformed sequences are reported as Unknown.
      val isScrollRelease = (button & 0x40) != 0 && releaseFinal
      MouseEvent.fromSgr(button, col, row, releaseFinal) match
        case Some(ev)                => InputKey.Mouse(ev)
        case None if isScrollRelease => InputKey.NoOp
        case None                    => InputKey.Unknown(s"CSI <${params.mkString(";")}$finalByte")

  /**
   * Read pasted bytes after the `ESC [ 200 ~` start marker, stopping when
   * the matching `ESC [ 201 ~` end marker arrives. Bytes between the two
   * markers — including embedded ESC characters that don't continue the
   * end marker — become the [[InputKey.Paste]] payload verbatim.
   */
  private def collectPaste(queueBridge: BlockingQueue[Integer]): InputKey =
    val EndMarker = "[201~"
    val sb        = new StringBuilder
    var matchPos  = 0
    var done      = false
    while !done do
      val n = queueBridge.take().intValue()
      if n == EndOfStream then
        queueBridge.put(EndOfStream)
        if matchPos > 0 then sb.append(EndMarker.substring(0, matchPos))
        done = true
      else
        val ch = n.toChar
        if ch == EndMarker.charAt(matchPos) then
          matchPos += 1
          if matchPos == EndMarker.length then done = true
        else
          // Mismatch: the partial-match prefix turns out to be content.
          if matchPos > 0 then
            sb.append(EndMarker.substring(0, matchPos))
            matchPos = 0
          // The non-matching byte itself might start a fresh match.
          if ch == EndMarker.charAt(0) then matchPos = 1
          else sb.append(ch)
    InputKey.Paste(sb.toString)

  /** Create a `TerminalKeySource` from a reader. */
  def apply(reader: Reader): TerminalKeySource =
    val bridge = new LinkedBlockingQueue[Integer]()

    // Producer: read raw ints from the reader
    val producerThread = ThreadUtils.startThread(() =>
      try
        @tailrec
        def loop(): Unit =
          val c = reader.read()
          if c == -1 then bridge.put(EndOfStream)
          else
            bridge.put(c)
            loop()
        loop()
      catch case _: InterruptedException => ()
    )

    val inputReads = new LinkedBlockingQueue[InputRead]()
    // Decoder: consume ints + escape sequences and emit InputKey
    val decoderThread = ThreadUtils.startThread(() =>
      try
        var pendingHigh: Int = -1 // high-surrogate code unit, -1 = none

        def flushPending(): Unit =
          if pendingHigh >= 0 then
            inputReads.put(InputRead.Key(InputKey.CharKey(pendingHigh.toChar)))
            pendingHigh = -1

        @tailrec
        def loop(continue: Boolean): Unit =
          if !continue then flushPending()
          else
            val c = bridge.take().intValue()
            if c == EndOfStream then
              flushPending()
              inputReads.put(InputRead.End)
              loop(false)
            else
              val key = processKey(c, bridge)
              key match
                case InputKey.CharKey(ch) if Character.isHighSurrogate(ch) =>
                  // Buffer the high surrogate; emit it only if the next
                  // character is *not* a matching low surrogate.
                  flushPending()
                  pendingHigh = ch.toInt
                case InputKey.CharKey(ch) if Character.isLowSurrogate(ch) && pendingHigh >= 0 =>
                  // Complete surrogate pair — emit as a single Supplementary.
                  val cp = Character.toCodePoint(pendingHigh.toChar, ch)
                  pendingHigh = -1
                  inputReads.put(InputRead.Key(InputKey.Supplementary(cp)))
                case InputKey.NoOp =>
                // Parser sentinel — drop but keep pending surrogate.
                case other =>
                  flushPending()
                  inputReads.put(InputRead.Key(other))
              loop(true)
        loop(true)
      catch {
        case _: InterruptedException => ()
      }
    )

    new TerminalKeySource:
      @volatile private var closed = false

      private def joinQuietly(t: Thread): Unit =
        try t.join(200L)
        catch {
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
        }

      override def next(): InputRead =
        try inputReads.take()
        catch
          case e: InterruptedException =>
            InputRead.Failed(e)

      override def close(): Try[Unit] =
        if !closed then
          closed = true
          producerThread.interrupt()
          decoderThread.interrupt()
          val closedReader = Try(reader.close())
          joinQuietly(producerThread)
          joinQuietly(decoderThread)
          closedReader
        else Success(())
