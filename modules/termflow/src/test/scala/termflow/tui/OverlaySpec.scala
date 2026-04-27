package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class OverlaySpec extends AnyFunSuite:

  // ---- OverlayPosition.resolve --------------------------------------------

  test("Centered places overlay at frame midpoint") {
    val (x, y) = OverlayPosition.resolve(OverlayPosition.Centered, 20, 4, 80, 24)
    assert(x.value == 31) // (80 - 20) / 2 + 1 = 31
    assert(y.value == 11) // (24 - 4)  / 2 + 1 = 11
  }

  test("TopLeft pins to (1+inset, 1+inset)") {
    val inset  = OverlayPosition.cornerInset
    val (x, y) = OverlayPosition.resolve(OverlayPosition.TopLeft, 10, 4, 80, 24)
    assert(x.value == 1 + inset)
    assert(y.value == 1 + inset)
  }

  test("BottomRight pins to (rootW - w - inset + 1, rootH - h - inset + 1)") {
    val inset  = OverlayPosition.cornerInset
    val (x, y) = OverlayPosition.resolve(OverlayPosition.BottomRight, 10, 4, 80, 24)
    assert(x.value == 80 - 10 - inset + 1) // = 70
    assert(y.value == 24 - 4 - inset + 1)  // = 20
  }

  test("At passes coordinates through unchanged") {
    val (x, y) = OverlayPosition.resolve(OverlayPosition.At(XCoord(5), YCoord(3)), 10, 4, 80, 24)
    assert(x.value == 5)
    assert(y.value == 3)
  }

  test("Oversized overlays clamp to (1, 1) instead of going negative") {
    val (x, y) = OverlayPosition.resolve(OverlayPosition.Centered, 200, 100, 80, 24)
    assert(x.value == 1)
    assert(y.value == 1)
  }

  // ---- buildFrame compositing ---------------------------------------------

  private val baseRoot: RootNode = RootNode(
    width = 40,
    height = 10,
    children = List(TextNode(XCoord(2), YCoord(2), List(Text("base", Style())))),
    input = None
  )

  test("RootNode.overlays default is empty (compat with existing call sites)") {
    assert(baseRoot.overlays == Nil)
  }

  test("an overlay paints over the base content at its resolved position") {
    val overlay = Overlay(
      position = OverlayPosition.At(XCoord(2), YCoord(2)),
      width = 4,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("OVER", Style()))))
    )
    val frame = AnsiRenderer.buildFrame(baseRoot.copy(overlays = List(overlay)))
    // Row 1 (0-indexed) starting at column 1 (0-indexed)
    val row     = frame.cells(1).map(_.ch).mkString
    val painted = row.substring(1, 5)
    assert(painted == "OVER", s"expected OVER overpaint at (2,2), got '$painted' in row '$row'")
  }

  test("non-modal overlay leaves base view input/cursor in place") {
    val overlay = Overlay(
      position = OverlayPosition.At(XCoord(10), YCoord(2)),
      width = 5,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("hint!", Style()))))
    )
    val withInput = baseRoot.copy(
      input = Some(InputNode(XCoord(2), YCoord(8), prompt = "ab", style = Style(), cursor = 1)),
      overlays = List(overlay)
    )
    val frame = AnsiRenderer.buildFrame(withInput)
    assert(frame.cursor.isDefined, "non-modal overlay must not suppress base cursor")
    assert(frame.cursor.get.x.value == 3, "cursor at base input column 2 + cursor index 1")
  }

  test("modal overlay suppresses the base view's input cursor") {
    val overlay = Overlay(
      position = OverlayPosition.At(XCoord(2), YCoord(2)),
      width = 5,
      height = 1,
      children = Nil,
      inputCapture = InputCapture.Modal
    )
    val withInput = baseRoot.copy(
      input = Some(InputNode(XCoord(2), YCoord(8), prompt = "ab", style = Style(), cursor = 1)),
      overlays = List(overlay)
    )
    val frame = AnsiRenderer.buildFrame(withInput)
    assert(frame.cursor.isEmpty, "modal overlay must suppress the base view's cursor")
  }

  test("modal overlay with its own input owns the cursor (translated to root coords)") {
    val overlay = Overlay(
      position = OverlayPosition.At(XCoord(10), YCoord(5)),
      width = 8,
      height = 3,
      children = Nil,
      input = Some(InputNode(XCoord(2), YCoord(2), prompt = "go", style = Style(), cursor = 2)),
      inputCapture = InputCapture.Modal
    )
    val frame = AnsiRenderer.buildFrame(baseRoot.copy(overlays = List(overlay)))
    assert(frame.cursor.isDefined, "overlay's own input should produce a cursor")
    val c = frame.cursor.get
    // overlay (10, 5) + input local (2, 2) → absolute (11, 6); cursor at index 2 → x = 13.
    assert(c.x.value == 13, s"expected x=13, got ${c.x.value}")
    assert(c.y.value == 6, s"expected y=6, got ${c.y.value}")
  }

  test("multiple overlays paint bottom-to-top in document order") {
    val low = Overlay(
      position = OverlayPosition.At(XCoord(2), YCoord(2)),
      width = 4,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("LOWX", Style()))))
    )
    val high = Overlay(
      position = OverlayPosition.At(XCoord(2), YCoord(2)),
      width = 4,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("HIGH", Style()))))
    )
    val frame   = AnsiRenderer.buildFrame(baseRoot.copy(overlays = List(low, high)))
    val painted = frame.cells(1).map(_.ch).mkString.substring(1, 5)
    assert(painted == "HIGH", s"top-of-stack overlay should win, got $painted")
  }

  test("renderPatch emits the overlay content after the base") {
    val overlay = Overlay(
      position = OverlayPosition.At(XCoord(2), YCoord(2)),
      width = 4,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("TOP!", Style(fg = Color.Red)))))
    )
    val out        = AnsiRenderer.renderPatch(baseRoot.copy(overlays = List(overlay)))
    val baseIdx    = out.indexOf("base")
    val overlayIdx = out.indexOf("TOP!")
    assert(baseIdx >= 0 && overlayIdx >= 0)
    assert(overlayIdx > baseIdx, "overlay text must come after base text in the render order")
  }
