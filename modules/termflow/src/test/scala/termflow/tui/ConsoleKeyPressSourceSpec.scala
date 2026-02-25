package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey

import java.io.StringReader
import java.util.concurrent.atomic.AtomicInteger

class ConsoleKeyPressSourceSpec extends AnyFunSuite:

  test("decodes printable character"):
    val source = ConsoleKeyPressSource(new StringReader("a"))
    try assert(source.next().get == InputKey.CharKey('a'))
    finally
      assert(source.close().isSuccess)
      ()

  test("decodes standalone ESC as Escape"):
    val source = ConsoleKeyPressSource(new StringReader("\u001b"))
    try assert(source.next().get == InputKey.Escape)
    finally
      assert(source.close().isSuccess)
      ()

  test("decodes ESC [ A as ArrowUp and close is idempotent"):
    val source = ConsoleKeyPressSource(new StringReader("\u001b[A"))
    assert(source.next().get == InputKey.ArrowUp)
    assert(source.close().isSuccess)
    assert(source.close().isSuccess)

  test("close closes underlying reader once"):
    final class CountingReader(data: String) extends StringReader(data):
      val closedCount = new AtomicInteger(0)
      override def close(): Unit =
        closedCount.incrementAndGet(): Unit
        super.close()

    val reader = new CountingReader("a")
    val source = ConsoleKeyPressSource(reader)
    assert(source.next().get == InputKey.CharKey('a'))
    assert(source.close().isSuccess)
    assert(source.close().isSuccess)
    assert(reader.closedCount.get() == 1)
