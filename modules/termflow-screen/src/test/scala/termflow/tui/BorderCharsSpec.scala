package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class BorderCharsSpec extends AnyFunSuite:

  test("sharp uses square corners") {
    assert(BorderChars.sharp.topLeft == '┌')
    assert(BorderChars.sharp.topRight == '┐')
    assert(BorderChars.sharp.bottomLeft == '└')
    assert(BorderChars.sharp.bottomRight == '┘')
    assert(BorderChars.sharp.horizontal == '─')
    assert(BorderChars.sharp.vertical == '│')
  }

  test("rounded uses rounded corners but the same edges") {
    assert(BorderChars.rounded.topLeft == '╭')
    assert(BorderChars.rounded.topRight == '╮')
    assert(BorderChars.rounded.bottomLeft == '╰')
    assert(BorderChars.rounded.bottomRight == '╯')
    assert(BorderChars.rounded.horizontal == '─')
    assert(BorderChars.rounded.vertical == '│')
  }

  test("double uses double-line glyphs throughout") {
    assert(BorderChars.double.topLeft == '╔')
    assert(BorderChars.double.bottomRight == '╝')
    assert(BorderChars.double.horizontal == '═')
    assert(BorderChars.double.vertical == '║')
  }

  test("ascii is portable to dumb terminals") {
    val all = Set(
      BorderChars.ascii.topLeft,
      BorderChars.ascii.topRight,
      BorderChars.ascii.bottomLeft,
      BorderChars.ascii.bottomRight,
      BorderChars.ascii.horizontal,
      BorderChars.ascii.vertical
    )
    assert(all.forall(_.toInt < 128), s"non-ASCII glyph in BorderChars.ascii: $all")
  }

  test("BoxNode defaults to sharp corners (preserves prior behaviour)") {
    val box: VNode.BoxNode = VNode.BoxNode(XCoord(1), YCoord(1), 4, 3, children = Nil, style = Style(border = true))
    assert(box.chars == BorderChars.sharp)
  }

  test("BoxNode accepts an explicit BorderChars") {
    val box: VNode.BoxNode = VNode.BoxNode(
      XCoord(1),
      YCoord(1),
      4,
      3,
      children = Nil,
      style = Style(border = true),
      chars = BorderChars.rounded
    )
    assert(box.chars == BorderChars.rounded)
  }
