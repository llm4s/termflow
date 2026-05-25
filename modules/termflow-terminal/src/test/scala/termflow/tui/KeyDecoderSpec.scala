package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey

class KeyDecoderSpec extends AnyFunSuite:

  test("decode Enter key — both LF (10) and CR (13)"):
    // Cooked terminals deliver Enter as LF (10); JLine raw mode disables
    // CR-to-LF translation and delivers CR (13). Both must decode to Enter
    // so dialogs/Prompt/History don't have to special-case Ctrl('M').
    assert(KeyDecoder.decode(10) == InputKey.Enter)
    assert(KeyDecoder.decode(13) == InputKey.Enter)

  test("decode Backspace key"):
    assert(KeyDecoder.decode(127) == InputKey.Backspace)

  test("decode printable ASCII characters"):
    assert(KeyDecoder.decode('a'.toInt) == InputKey.CharKey('a'))
    assert(KeyDecoder.decode('z'.toInt) == InputKey.CharKey('z'))
    assert(KeyDecoder.decode('A'.toInt) == InputKey.CharKey('A'))
    assert(KeyDecoder.decode('Z'.toInt) == InputKey.CharKey('Z'))
    assert(KeyDecoder.decode('0'.toInt) == InputKey.CharKey('0'))
    assert(KeyDecoder.decode('9'.toInt) == InputKey.CharKey('9'))
    assert(KeyDecoder.decode(' '.toInt) == InputKey.CharKey(' '))
    assert(KeyDecoder.decode('!'.toInt) == InputKey.CharKey('!'))
    assert(KeyDecoder.decode('~'.toInt) == InputKey.CharKey('~'))

  test("decode control characters Ctrl+A through Ctrl+Z"):
    assert(KeyDecoder.decode(1) == InputKey.Ctrl('A'))
    assert(KeyDecoder.decode(3) == InputKey.Ctrl('C'))
    assert(KeyDecoder.decode(4) == InputKey.Ctrl('D'))
    assert(KeyDecoder.decode(26) == InputKey.Ctrl('Z'))

  test("decode Tab as InputKey.Tab — not Ctrl('I')"):
    // ASCII 9 (HT) is the byte produced by both Tab and Ctrl+I; we
    // surface it as Tab so focus dispatchers don't have to know the
    // encoding detail.
    assert(KeyDecoder.decode(9) == InputKey.Tab)

  test("decode unknown codes"):
    assert(KeyDecoder.decode(0) == InputKey.Unknown("0"))
    assert(KeyDecoder.decode(200) == InputKey.CharKey(200.toChar)) // U+00C8 (È)
    assert(KeyDecoder.decode(-1) == InputKey.Unknown("-1"))

  test("decode boundary values"):
    // Code 31 is below printable range
    assert(KeyDecoder.decode(31) == InputKey.Unknown("31"))
    // Code 32 is space (first printable)
    assert(KeyDecoder.decode(32) == InputKey.CharKey(' '))
    // Code 126 is tilde (last printable)
    assert(KeyDecoder.decode(126) == InputKey.CharKey('~'))
    // Code 127 is backspace
    assert(KeyDecoder.decode(127) == InputKey.Backspace)
    // Code 128 is beyond printable
    // Code 128 (0x80) is the PAD control (U+0080), now decoded as CharKey
    assert(KeyDecoder.decode(128) == InputKey.CharKey(128.toChar))

  test("decode non-ASCII BMP characters"):
    // Arabic ش (U+0634)
    assert(KeyDecoder.decode(0x0634) == InputKey.CharKey(0x0634.toChar))
    // CJK 中 (U+4E2D)
    assert(KeyDecoder.decode(0x4E2D) == InputKey.CharKey(0x4E2D.toChar))
    // Cyrillic Я (U+042F)
    assert(KeyDecoder.decode(0x042F) == InputKey.CharKey(0x042F.toChar))
    // Latin é (U+00E9)
    assert(KeyDecoder.decode(0x00E9) == InputKey.CharKey(0x00E9.toChar))

  test("Modifiers.isEmpty / nonEmpty are mutually exclusive"):
    assert(KeyDecoder.Modifiers.none.isEmpty)
    assert(!KeyDecoder.Modifiers.none.nonEmpty)
    val held = KeyDecoder.Modifiers(ctrl = true)
    assert(!held.isEmpty)
    assert(held.nonEmpty)
