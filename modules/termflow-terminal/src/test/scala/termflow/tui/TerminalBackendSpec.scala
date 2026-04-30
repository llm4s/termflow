package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.io.Reader
import java.io.StringReader
import java.io.StringWriter
import java.io.Writer

class TerminalBackendSpec extends AnyFunSuite:

  private def makeBackend(
    out: StringWriter = new StringWriter(),
    caps: Capabilities = Capabilities.default
  ): TerminalBackend =
    new TerminalBackend:
      override def reader: Reader             = new StringReader("")
      override def writer: Writer             = out
      override def width: Int                 = 10
      override def height: Int                = 5
      override def close(): Unit              = ()
      override def capabilities: Capabilities = caps

  test("default trait write delegates to the writer and flush flushes it"):
    val out     = new StringWriter()
    val backend = makeBackend(out = out)
    backend.write("hello")
    backend.flush()
    assert(out.toString == "hello")

  test("requestAttention is a no-op when notifications are Disabled"):
    val out = new StringWriter()
    val backend = makeBackend(
      out = out,
      caps = Capabilities.default.copy(notifications = NotificationKind.Disabled)
    )
    backend.requestAttention()
    assert(out.toString.isEmpty)

  test("requestAttention writes the BellOnly escape and flushes"):
    val out = new StringWriter()
    val backend = makeBackend(
      out = out,
      caps = Capabilities.default.copy(notifications = NotificationKind.BellOnly)
    )
    backend.requestAttention()
    assert(out.toString == "")

  test("notify writes the OSC envelope when notifications are enabled"):
    val out = new StringWriter()
    val backend = makeBackend(
      out = out,
      caps = Capabilities.default.copy(notifications = NotificationKind.ITerm2)
    )
    backend.notify("Build", "done")
    val written = out.toString
    assert(written.contains("Build: done"))
    assert(written.startsWith("]9;"))

  test("notify is a no-op when notifications are Disabled"):
    val out = new StringWriter()
    val backend = makeBackend(
      out = out,
      caps = Capabilities.default.copy(notifications = NotificationKind.Disabled)
    )
    backend.notify("Build", "done")
    assert(out.toString.isEmpty)

  test("default onResize returns None"):
    assert(makeBackend().onResize(() => ()).isEmpty)

  test("default capabilities is Capabilities.default"):
    val backend = new TerminalBackend:
      override def reader: Reader = new StringReader("")
      override def writer: Writer = new StringWriter()
      override def width: Int     = 1
      override def height: Int    = 1
      override def close(): Unit  = ()
    assert(backend.capabilities == Capabilities.default)
