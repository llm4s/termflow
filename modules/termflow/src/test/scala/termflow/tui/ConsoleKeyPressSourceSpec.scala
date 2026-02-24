package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey

import java.io.StringReader

class ConsoleKeyPressSourceSpec extends AnyFunSuite:

  test("decodes printable character"):
    val source = ConsoleKeyPressSource(new StringReader("a"))
    try
      assert(source.next().get == InputKey.CharKey('a'))
    finally source.close()

  test("decodes standalone ESC as Escape"):
    val source = ConsoleKeyPressSource(new StringReader("\u001b"))
    try
      assert(source.next().get == InputKey.Escape)
    finally source.close()

  test("decodes ESC [ A as ArrowUp and close is idempotent"):
    val source = ConsoleKeyPressSource(new StringReader("\u001b[A"))
    assert(source.next().get == InputKey.ArrowUp)
    source.close()
    source.close()
