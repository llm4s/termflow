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
    assert(KeyDecoder.decode(200) == InputKey.Unknown("200"))
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
    assert(KeyDecoder.decode(128) == InputKey.Unknown("128"))

  test("Modifiers.isEmpty / nonEmpty are mutually exclusive"):
    assert(KeyDecoder.Modifiers.none.isEmpty)
    assert(!KeyDecoder.Modifiers.none.nonEmpty)
    val held = KeyDecoder.Modifiers(ctrl = true)
    assert(!held.isEmpty)
    assert(held.nonEmpty)
