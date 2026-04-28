package termflow.testkit

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.KeyDecoder.Modifiers

class KeySimSpec extends AnyFunSuite:

  test("char wraps a printable character") {
    assert(KeySim.char('a') == InputKey.CharKey('a'))
  }

  test("ctrl produces a Ctrl key event") {
    assert(KeySim.ctrl('C') == InputKey.Ctrl('C'))
  }

  test("named key fields are exact aliases of the InputKey cases") {
    assert(KeySim.Enter == InputKey.Enter)
    assert(KeySim.Escape == InputKey.Escape)
    assert(KeySim.Backspace == InputKey.Backspace)
    assert(KeySim.Delete == InputKey.Delete)
    assert(KeySim.Insert == InputKey.Insert)
    assert(KeySim.Home == InputKey.Home)
    assert(KeySim.End == InputKey.End)
    assert(KeySim.PageUp == InputKey.PageUp)
    assert(KeySim.PageDown == InputKey.PageDown)
    assert(KeySim.ArrowUp == InputKey.ArrowUp)
    assert(KeySim.ArrowDown == InputKey.ArrowDown)
    assert(KeySim.ArrowLeft == InputKey.ArrowLeft)
    assert(KeySim.ArrowRight == InputKey.ArrowRight)
    assert(KeySim.BackTab == InputKey.BackTab)
    assert(KeySim.Tab == InputKey.Tab)
  }

  test("f produces F1..F12 cases") {
    val expected = Vector(
      InputKey.F1,
      InputKey.F2,
      InputKey.F3,
      InputKey.F4,
      InputKey.F5,
      InputKey.F6,
      InputKey.F7,
      InputKey.F8,
      InputKey.F9,
      InputKey.F10,
      InputKey.F11,
      InputKey.F12
    )
    (1 to 12).foreach(i => assert(KeySim.f(i) == expected(i - 1), s"F$i mismatch"))
  }

  test("f rejects out-of-range values") {
    intercept[IllegalArgumentException](KeySim.f(0))
    intercept[IllegalArgumentException](KeySim.f(13))
  }

  test("paste preserves the literal payload, including embedded newlines") {
    val payload = "first\nsecond\nthird"
    assert(KeySim.paste(payload) == InputKey.Paste(payload))
  }

  test("modified returns the bare key when no flags are set") {
    assert(KeySim.modified(InputKey.ArrowUp) == InputKey.ArrowUp)
  }

  test("modified wraps a single flag") {
    val k = KeySim.modified(InputKey.ArrowRight, ctrl = true)
    assert(k == InputKey.Modified(InputKey.ArrowRight, Modifiers(ctrl = true)))
  }

  test("modified merges flags into an existing Modified envelope") {
    val first  = KeySim.shift(InputKey.ArrowDown)
    val second = KeySim.modified(first, ctrl = true)
    second match
      case InputKey.Modified(inner, mods) =>
        assert(inner == InputKey.ArrowDown)
        assert(mods.shift && mods.ctrl)
      case other => fail(s"expected Modified, got $other")
  }

  test("ctrlShift wraps both modifiers") {
    val k = KeySim.ctrlShift(InputKey.ArrowLeft)
    k match
      case InputKey.Modified(inner, mods) =>
        assert(inner == InputKey.ArrowLeft)
        assert(mods.ctrl && mods.shift)
        assert(!mods.alt && !mods.meta)
      case other => fail(s"expected Modified, got $other")
  }

  test("typeString turns a plain string into one CharKey per character") {
    val keys = KeySim.typeString("hi")
    assert(keys == Vector(InputKey.CharKey('h'), InputKey.CharKey('i')))
  }

  test("typeString turns tab into InputKey.Tab") {
    val keys = KeySim.typeString("a\tb")
    assert(keys == Vector(InputKey.CharKey('a'), InputKey.Tab, InputKey.CharKey('b')))
  }

  test("typeString turns LF into Enter") {
    val keys = KeySim.typeString("a\nb")
    assert(keys == Vector(InputKey.CharKey('a'), InputKey.Enter, InputKey.CharKey('b')))
  }

  test("typeString collapses CRLF into a single Enter event") {
    val keys = KeySim.typeString("a\r\nb")
    assert(keys == Vector(InputKey.CharKey('a'), InputKey.Enter, InputKey.CharKey('b')))
  }

  test("typeString turns a bare CR into Enter") {
    val keys = KeySim.typeString("a\rb")
    assert(keys == Vector(InputKey.CharKey('a'), InputKey.Enter, InputKey.CharKey('b')))
  }

  test("typeString of an empty string yields no events") {
    assert(KeySim.typeString("").isEmpty)
  }
