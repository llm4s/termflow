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

  test("buildFrame: overlay rectangle wipes cells beneath it (no background leakage)") {
    // Base frame with a long line of `panel-content` across the middle row.
    val crowdedBase = RootNode(
      width = 30,
      height = 5,
      children = List(TextNode(XCoord(1), YCoord(2), List(Text("XXXXXXXXXXXXXXXXXXXXXXXXXXXX", Style())))),
      input = None
    )
    // Overlay covers cols 5..14 on row 2 with a single TextNode + a 1×3 box.
    val overlay = Overlay(
      position = OverlayPosition.At(XCoord(5), YCoord(2)),
      width = 10,
      height = 3,
      children = List(VNode.BoxNode(XCoord(1), YCoord(1), 10, 3, Nil, Style(border = true)))
    )
    val frame = AnsiRenderer.buildFrame(crowdedBase.copy(overlays = List(overlay)))

    // Cells inside the overlay rectangle but outside the border (col 6..13 on row 2)
    // must be blank — no `X` from the underlying panel should leak through.
    val rowText  = frame.cells(1).map(_.ch).mkString
    val interior = rowText.substring(5, 13) // 0-based slice of the overlay interior
    assert(!interior.contains('X'), s"overlay interior leaked underlying X chars: '$interior'")

    // Cells outside the overlay rectangle still hold the panel content.
    assert(rowText.charAt(0) == 'X' && rowText.charAt(15) == 'X', s"non-overlay cells should remain: '$rowText'")
  }

  test("renderPatch: overlay clears its rectangle before drawing children") {
    val crowdedBase = RootNode(
      width = 30,
      height = 5,
      children = List(TextNode(XCoord(1), YCoord(2), List(Text("LEAK-LEAK-LEAK-LEAK", Style())))),
      input = None
    )
    val overlay = Overlay(
      position = OverlayPosition.At(XCoord(5), YCoord(2)),
      width = 10,
      height = 1,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("DLG", Style()))))
    )
    val out      = AnsiRenderer.renderPatch(crowdedBase.copy(overlays = List(overlay)))
    val leakIdx  = out.indexOf("LEAK")
    val blankIdx = out.indexOf("          ", leakIdx) // 10 spaces written by the wipe
    val dlgIdx   = out.indexOf("DLG")
    assert(leakIdx >= 0, "base text should still appear in patch")
    assert(blankIdx > leakIdx, "wipe blanks must come after the base content")
    assert(dlgIdx > blankIdx, "overlay glyphs must come after the wipe")
  }
