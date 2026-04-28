package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*

class SpinnerSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private def firstChar(v: VNode): Char =
    AnsiRenderer.buildFrame(RootNode(1, 1, List(v), None)).cells(0)(0).ch

  test("frame index cycles modulo the frame count"):
    val v0 = Spinner(Spinner.Line, frame = 0)
    val v4 = Spinner(Spinner.Line, frame = 4)
    val v8 = Spinner(Spinner.Line, frame = 8)
    assert(firstChar(v0) == firstChar(v4)) // 0 mod 4 = 4 mod 4 = 0
    assert(firstChar(v4) == firstChar(v8))

  test("frame advances between consecutive ticks"):
    val a = firstChar(Spinner(Spinner.Line, frame = 0))
    val b = firstChar(Spinner(Spinner.Line, frame = 1))
    assert(a != b)

  test("negative frames are handled by wrapping into range"):
    val v = Spinner(Spinner.Line, frame = -1)
    // -1 maps to index 3 (the last frame)
    assert(firstChar(v) == Spinner.Line(3).charAt(0))

  test("empty frames set is rejected with a clear error"):
    val ex = intercept[IllegalArgumentException] {
      Spinner(Vector.empty, frame = 0)
    }
    assert(ex.getMessage.contains("must be non-empty"))

  test("Spinner uses theme.primary for the frame character"):
    val v     = Spinner(Spinner.Line, frame = 0)
    val cells = AnsiRenderer.buildFrame(RootNode(1, 1, List(v), None)).cells
    assert(cells(0)(0).style.fg == Theme.dark.primary)

  test("Spinner.width reports the widest frame in a set"):
    assert(Spinner.width(Spinner.Line) == 1)
    assert(Spinner.width(Spinner.Dots) == 3)
    assert(Spinner.width(Vector.empty) == 0)

  test("braille spinner renders the expected character for a known frame"):
    val v     = Spinner(Spinner.Braille, frame = 3)
    val cells = AnsiRenderer.buildFrame(RootNode(1, 1, List(v), None)).cells
    assert(cells(0)(0).ch == Spinner.Braille(3).charAt(0))
