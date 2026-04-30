package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.InputRead.Key
import termflow.tui.KeyDecoder.InputKey

import java.io.StringReader
import java.util.concurrent.atomic.AtomicInteger

class ConsoleKeyPressSourceSpec extends AnyFunSuite:

  test("decodes printable character"):
    val source = ConsoleKeyPressSource(new StringReader("a"))
    try assert(source.next() == Key(InputKey.CharKey('a')))
    finally
      assert(source.close().isSuccess)
      ()

  test("decodes standalone ESC as Escape"):
    val source = ConsoleKeyPressSource(new StringReader("\u001b"))
    try assert(source.next() == Key(InputKey.Escape))
    finally
      assert(source.close().isSuccess)
      ()

  test("decodes ESC [ A as ArrowUp and close is idempotent"):
    val source = ConsoleKeyPressSource(new StringReader("\u001b[A"))
    assert(source.next() == Key(InputKey.ArrowUp))
    assert(source.close().isSuccess)
    assert(source.close().isSuccess)

  test("decodes ESC [ Z as BackTab (Shift+Tab)"):
    val source = ConsoleKeyPressSource(new StringReader("\u001b[Z"))
    try assert(source.next() == Key(InputKey.BackTab))
    finally
      assert(source.close().isSuccess)
      ()

  test("close closes underlying reader once"):
    final class CountingReader(data: String) extends StringReader(data):
      val closedCount = new AtomicInteger(0)
      override def close(): Unit =
        closedCount.incrementAndGet(): Unit
        super.close()

    val reader = new CountingReader("a")
    val source = ConsoleKeyPressSource(reader)
    assert(source.next() == Key(InputKey.CharKey('a')))
    assert(source.close().isSuccess)
    assert(source.close().isSuccess)
    assert(reader.closedCount.get() == 1)

  // ---- Extended modifier parsing (Stage 2 §5.6) ----

  private def decode(input: String): InputKey =
    val source = ConsoleKeyPressSource(new StringReader(input))
    try
      source.next() match
        case Key(k) => k
        case other  => fail(s"expected Key, got $other")
    finally
      assert(source.close().isSuccess)
      ()

  test("decodes Shift+ArrowUp (CSI 1;2 A) as Modified(ArrowUp, shift)"):
    val key = decode("[1;2A")
    assert(key == InputKey.Modified(InputKey.ArrowUp, KeyDecoder.Modifiers(shift = true)))

  test("decodes Ctrl+ArrowRight (CSI 1;5 C)"):
    val key = decode("[1;5C")
    assert(key == InputKey.Modified(InputKey.ArrowRight, KeyDecoder.Modifiers(ctrl = true)))

  test("decodes Shift+Ctrl+ArrowLeft (CSI 1;6 D)"):
    val key = decode("[1;6D")
    assert(key == InputKey.Modified(InputKey.ArrowLeft, KeyDecoder.Modifiers(shift = true, ctrl = true)))

  test("decodes Shift+F5 (CSI 15;2 ~)"):
    val key = decode("[15;2~")
    assert(key == InputKey.Modified(InputKey.F5, KeyDecoder.Modifiers(shift = true)))

  test("decodes Ctrl+Delete (CSI 3;5 ~)"):
    val key = decode("[3;5~")
    assert(key == InputKey.Modified(InputKey.Delete, KeyDecoder.Modifiers(ctrl = true)))

  test("decodes Insert / PageUp / PageDown / Home / End from CSI tilde forms"):
    assert(decode("[2~") == InputKey.Insert)
    assert(decode("[5~") == InputKey.PageUp)
    assert(decode("[6~") == InputKey.PageDown)
    assert(decode("[1~") == InputKey.Home)
    assert(decode("[4~") == InputKey.End)

  test("CSI without modifier still decodes to bare key (no Modified wrapper)"):
    assert(decode("[A") == InputKey.ArrowUp)
    assert(decode("[H") == InputKey.Home)
    assert(decode("[3~") == InputKey.Delete)

  test("decodes Alt+character (ESC + char) as Modified(CharKey, alt)"):
    val key = decode("a")
    assert(key == InputKey.Modified(InputKey.CharKey('a'), KeyDecoder.Modifiers(alt = true)))

  // ---- Bracketed paste (Stage 2 §5.4) ----

  test("CSI 200~ ... CSI 201~ collapses to a single InputKey.Paste"):
    val input = "[200~hello world[201~"
    assert(decode(input) == InputKey.Paste("hello world"))

  test("paste content preserves embedded newlines"):
    val input = "[200~line one\nline two\r\nline three[201~"
    assert(decode(input) == InputKey.Paste("line one\nline two\r\nline three"))

  test("paste with a stray ESC inside reads through to the real end marker"):
    // A literal ESC character inside paste content (not followed by [201~)
    // becomes part of the payload rather than ending the paste.
    val input = "[200~beforeabc[201~"
    assert(decode(input) == InputKey.Paste("beforeabc"))

  test("orphan paste-end (no preceding start) is ignored"):
    val input = "[201~"
    decode(input) match
      case InputKey.Unknown(_) => ()
      case other               => fail(s"expected Unknown, got $other")

  test("paste containing the start marker prefix-then-mismatch keeps content intact"):
    // Content includes `[200` followed by `X` — the partial match resets
    // and the bytes are emitted as content.
    val input = "[200~head[200X[201~"
    assert(decode(input) == InputKey.Paste("head[200X"))

  // ---- Mouse / SGR-1006 (Stage 2 §5.1) ----

  test("CSI <0;col;row M decodes as left-button press at (col,row)"):
    val key = decode("\u001b[<0;7;3M")
    assert(key == InputKey.Mouse(MouseEvent.Press(MouseButton.Left, 7, 3, KeyDecoder.Modifiers())))

  test("CSI <0;col;row m decodes as left-button release"):
    val key = decode("\u001b[<0;7;3m")
    assert(key == InputKey.Mouse(MouseEvent.Release(MouseButton.Left, 7, 3, KeyDecoder.Modifiers())))

  test("CSI <2;col;row M decodes as right-button press"):
    val key = decode("\u001b[<2;5;5M")
    assert(key == InputKey.Mouse(MouseEvent.Press(MouseButton.Right, 5, 5, KeyDecoder.Modifiers())))

  test("CSI <32;col;row M decodes as left-button drag (motion bit set)"):
    val key = decode("\u001b[<32;10;4M")
    assert(key == InputKey.Mouse(MouseEvent.Drag(MouseButton.Left, 10, 4, KeyDecoder.Modifiers())))

  test("CSI <35;col;row M decodes as bare Move (button code 3 = none)"):
    val key = decode("\u001b[<35;12;7M")
    assert(key == InputKey.Mouse(MouseEvent.Move(12, 7, KeyDecoder.Modifiers())))

  test("CSI <64;col;row M decodes as scroll up"):
    val key = decode("\u001b[<64;3;3M")
    assert(key == InputKey.Mouse(MouseEvent.Scroll(ScrollDirection.Up, 3, 3, KeyDecoder.Modifiers())))

  test("CSI <65;col;row M decodes as scroll down"):
    val key = decode("\u001b[<65;3;3M")
    assert(key == InputKey.Mouse(MouseEvent.Scroll(ScrollDirection.Down, 3, 3, KeyDecoder.Modifiers())))

  test("Ctrl+Shift+Left-press carries the modifier bits"):
    // button = 0 (left) + 4 (shift) + 16 (ctrl) = 20
    val key = decode("\u001b[<20;1;1M")
    assert(
      key == InputKey.Mouse(
        MouseEvent.Press(MouseButton.Left, 1, 1, KeyDecoder.Modifiers(shift = true, ctrl = true))
      )
    )

  test("malformed mouse sequence is reported as Unknown"):
    decode("\u001b[<99M") match
      case InputKey.Unknown(_) => ()
      case other               => fail(s"expected Unknown, got $other")

  test("scroll release byte (`m`) is silently dropped — no double-fire") {
    // Some terminals emit M *and* m for each wheel detent; the m must not
    // surface as a duplicate Scroll event. Source emits only the press.
    val source = ConsoleKeyPressSource(new StringReader("\u001b[<64;3;3M\u001b[<64;3;3m"))
    try
      val first = source.next()
      assert(first == Key(InputKey.Mouse(MouseEvent.Scroll(ScrollDirection.Up, 3, 3, KeyDecoder.Modifiers()))))
      val second = source.next()
      assert(
        second == InputRead.End,
        s"release byte should be filtered; expected End after one event, got $second"
      )
    finally
      assert(source.close().isSuccess)
      ()
  }

  test("standalone scroll-release (no preceding press) is also dropped") {
    val source = ConsoleKeyPressSource(new StringReader("\u001b[<64;3;3m"))
    try
      val read = source.next()
      assert(read == InputRead.End, s"orphan release should be filtered, got $read")
    finally
      assert(source.close().isSuccess)
      ()
  }

  test("Modifiers.fromXtermCode encodes the xterm bitmap"):
    import KeyDecoder.Modifiers
    assert(Modifiers.fromXtermCode(2) == Modifiers(shift = true))
    assert(Modifiers.fromXtermCode(3) == Modifiers(alt = true))
    assert(Modifiers.fromXtermCode(4) == Modifiers(shift = true, alt = true))
    assert(Modifiers.fromXtermCode(5) == Modifiers(ctrl = true))
    assert(Modifiers.fromXtermCode(6) == Modifiers(shift = true, ctrl = true))
    assert(Modifiers.fromXtermCode(7) == Modifiers(alt = true, ctrl = true))
    assert(Modifiers.fromXtermCode(8) == Modifiers(shift = true, alt = true, ctrl = true))
    assert(Modifiers.fromXtermCode(9) == Modifiers(meta = true))
    // Out-of-range bytes decode as `none` rather than raising.
    assert(Modifiers.fromXtermCode(0) == Modifiers.none)
    assert(Modifiers.fromXtermCode(1) == Modifiers.none)
    assert(Modifiers.fromXtermCode(99) == Modifiers.none)

  // ---- SS3 sequences (ESC O <letter>) ----

  test("decodes SS3 F1..F4 (ESC O P/Q/R/S)"):
    assert(decode("OP") == InputKey.F1)
    assert(decode("OQ") == InputKey.F2)
    assert(decode("OR") == InputKey.F3)
    assert(decode("OS") == InputKey.F4)

  test("decodes SS3 Home / End (ESC O H / ESC O F)"):
    assert(decode("OH") == InputKey.Home)
    assert(decode("OF") == InputKey.End)

  test("unrecognised SS3 letter is reported as Unknown"):
    decode("OZ") match
      case InputKey.Unknown(_) => ()
      case other               => fail(s"expected Unknown, got $other")

  test("SS3 with no following byte (truncated) decodes as Escape"):
    // Bare ESC O with no follow-up byte falls through the SS3 timeout
    // and should surface as Escape (the standalone-ESC fallback).
    val src = ConsoleKeyPressSource(new StringReader("O"))
    try
      // Two reads possible: Escape (from standalone-ESC), then End or
      // similar for the trailing 'O'. We just check the first.
      val first = src.next()
      first match
        case InputRead.Key(InputKey.Escape) => ()
        case other                          => fail(s"expected Escape from truncated SS3, got $other")
    finally
      assert(src.close().isSuccess)
      ()

  // ---- CSI F1..F4 with letter final byte ----

  test("decodes F1..F4 via the CSI letter form (CSI 1;<mod> P/Q/R/S)"):
    assert(decode("[P") == InputKey.F1)
    assert(decode("[Q") == InputKey.F2)
    assert(decode("[R") == InputKey.F3)
    assert(decode("[S") == InputKey.F4)
    assert(decode("[1;2P") == InputKey.Modified(InputKey.F1, KeyDecoder.Modifiers(shift = true)))

  // ---- All CSI tilde keys ----

  test("decodes the full CSI ~ table (Home, Insert, PageUp/Down, F1..F12)"):
    val cases: Seq[(String, KeyDecoder.InputKey)] = Seq(
      "[1~"  -> InputKey.Home,
      "[7~"  -> InputKey.Home,
      "[2~"  -> InputKey.Insert,
      "[3~"  -> InputKey.Delete,
      "[4~"  -> InputKey.End,
      "[8~"  -> InputKey.End,
      "[5~"  -> InputKey.PageUp,
      "[6~"  -> InputKey.PageDown,
      "[11~" -> InputKey.F1,
      "[12~" -> InputKey.F2,
      "[13~" -> InputKey.F3,
      "[14~" -> InputKey.F4,
      "[15~" -> InputKey.F5,
      "[17~" -> InputKey.F6,
      "[18~" -> InputKey.F7,
      "[19~" -> InputKey.F8,
      "[20~" -> InputKey.F9,
      "[21~" -> InputKey.F10,
      "[23~" -> InputKey.F11,
      "[24~" -> InputKey.F12
    )
    cases.foreach { case (input, expected) =>
      assert(decode(input) == expected, s"input ${input.replace("", "ESC")} expected $expected")
    }

  test("CSI tilde with an unknown number is reported as Unknown"):
    decode("[99~") match
      case InputKey.Unknown(_) => ()
      case other               => fail(s"expected Unknown, got $other")

  // ---- Other CSI / unknown finals ----

  test("CSI with an unrecognised final byte is reported as Unknown"):
    decode("[99X") match
      case InputKey.Unknown(_) => ()
      case other               => fail(s"expected Unknown, got $other")

  test("CSI with a non-standard prefix (?, >, !) is reported as Unknown"):
    decode("[?1;2c") match
      case InputKey.Unknown(_) => ()
      case other               => fail(s"expected Unknown, got $other")

  // ---- Paste content reaching EOF before the end marker ----

  test("end-of-stream during paste collection flushes any partial match"):
    // Stream ends inside what looked like the start of an end marker:
    // the partial-match prefix should be flushed into the paste payload,
    // and the source must surface InputRead.End next.
    val src = ConsoleKeyPressSource(new StringReader("[200~hi[20"))
    try
      val read = src.next()
      read match
        case InputRead.Key(InputKey.Paste(content)) =>
          assert(content.startsWith("hi"))
        case other => fail(s"expected Paste, got $other")
    finally
      assert(src.close().isSuccess)
      ()
