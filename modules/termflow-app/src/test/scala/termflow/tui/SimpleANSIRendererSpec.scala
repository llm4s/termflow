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
