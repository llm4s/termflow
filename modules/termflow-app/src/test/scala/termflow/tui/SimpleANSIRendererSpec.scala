package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class SimpleANSIRendererSpec extends AnyFunSuite:

  private def buildFrame(): AnsiRenderer.RenderFrame =
    AnsiRenderer.buildFrame(
      RootNode(
        width = 20,
        height = 5,
        children = List(TextNode(XCoord(1), YCoord(1), List(Text("hello", Style())))),
        input = None
      )
    )

  test("formatErrorBanner formats every TermFlowError variant"):
    assert(SimpleANSIRenderer.formatErrorBanner(TermFlowError.Validation("bad")) == "Invalid input: bad")
    assert(SimpleANSIRenderer.formatErrorBanner(TermFlowError.ConfigError("nope")) == "Config error: nope")
    assert(SimpleANSIRenderer.formatErrorBanner(TermFlowError.Unexpected("boom", None)) == "Error: boom")
    assert(SimpleANSIRenderer.formatErrorBanner(TermFlowError.CommandError(":foo")) == "Unrecognised command: :foo")
    assert(SimpleANSIRenderer.formatErrorBanner(TermFlowError.UnknownApp("counter")) == "Unknown app: counter")
    assert(SimpleANSIRenderer.formatErrorBanner(TermFlowError.ModelNotFound) == "Model not found")

  test("overlayErrorBanner stamps a red bold banner across the top row"):
    val frame   = buildFrame()
    val overlay = SimpleANSIRenderer.overlayErrorBanner(frame, TermFlowError.Validation("oops"))

    val rowText = overlay.cells(0).map(_.ch).mkString
    assert(rowText.startsWith(" Invalid input: oops"))
    assert(rowText.length == frame.width)

    val first = overlay.cells(0)(1)
    assert(first.style.bold)
    assert(first.style.fg == Color.White)
    assert(first.style.bg == Color.Red)

  test("overlayErrorBanner truncates messages that overflow the width"):
    val frame   = buildFrame() // width = 20, banner budget = 18
    val long    = "x" * 50
    val overlay = SimpleANSIRenderer.overlayErrorBanner(frame, TermFlowError.Unexpected(long, None))
    val rowText = overlay.cells(0).map(_.ch).mkString
    assert(rowText.length == 20)
    assert(rowText.endsWith("…"))

  test("overlayErrorBanner is a no-op for zero-sized frames"):
    val empty = AnsiRenderer.RenderFrame(width = 0, height = 0, cells = Array.empty, cursor = None)
    val out   = SimpleANSIRenderer.overlayErrorBanner(empty, TermFlowError.Validation("x"))
    assert(out eq empty)

  test("overlayErrorBanner does not mutate the input frame"):
    val frame  = buildFrame()
    val before = frame.cells(0).map(_.ch).mkString
    val _      = SimpleANSIRenderer.overlayErrorBanner(frame, TermFlowError.Validation("x"))
    assert(frame.cells(0).map(_.ch).mkString == before)

  // ---- render() integration ----

  import java.io.Reader
  import java.io.StringReader
  import java.io.StringWriter
  import java.nio.file.Path

  private def metrics(enabled: Boolean): RenderMetrics =
    val cfg = MetricsConfig(enabled = enabled)
    val log = FrameworkLog(LoggingConfig(LogPath(Path.of("target", "termflow-renderer-spec.log"))))
    new RenderMetrics(cfg, log)

  private def newBackend(): TerminalBackend =
    new TerminalBackend:
      val out                     = new StringWriter()
      override def reader: Reader = new StringReader("")
      override def writer         = out
      override def width: Int     = 30
      override def height: Int    = 10
      override def close(): Unit  = ()

  test("render writes ANSI for the first frame and records metrics when enabled"):
    val r       = SimpleANSIRenderer()
    val backend = newBackend()
    val m       = metrics(enabled = true)
    val root = RootNode(
      width = 10,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("hello", Style())))),
      input = None
    )
    r.render(root, err = None, terminal = backend, renderMetrics = m)
    val first = backend.writer.asInstanceOf[StringWriter].toString
    assert(first.contains("hello"))
    assert(m.isEnabled)

  test("render emits the error banner text when the runtime supplies an err"):
    val r       = SimpleANSIRenderer()
    val backend = newBackend()
    val m       = metrics(enabled = false)
    val root = RootNode(
      width = 30,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("hi", Style())))),
      input = None
    )
    r.render(root, err = Some(TermFlowError.Validation("nope")), terminal = backend, renderMetrics = m)
    val out = backend.writer.asInstanceOf[StringWriter].toString
    assert(out.contains("Invalid input: nope"))

  test("an identical second render emits no further ANSI"):
    val r       = SimpleANSIRenderer()
    val backend = newBackend()
    val m       = metrics(enabled = false)
    val root = RootNode(
      width = 10,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("steady", Style())))),
      input = None
    )
    r.render(root, None, backend, m)
    val before = backend.writer.asInstanceOf[StringWriter].toString.length
    r.render(root, None, backend, m)
    val after = backend.writer.asInstanceOf[StringWriter].toString.length
    assert(after == before)

  test("a frame resize forces a full repaint with clear-screen + home-cursor"):
    val r       = SimpleANSIRenderer()
    val backend = newBackend()
    val m       = metrics(enabled = false)
    val first   = RootNode(width = 10, height = 3, children = Nil, input = None)
    val second  = RootNode(width = 18, height = 6, children = Nil, input = None)
    r.render(first, None, backend, m)
    val asWriter = backend.writer.asInstanceOf[StringWriter]
    asWriter.getBuffer.setLength(0)
    r.render(second, None, backend, m)
    val out = asWriter.toString
    assert(out.contains(ANSI.clearScreen))
    assert(out.contains(ANSI.homeCursor))
