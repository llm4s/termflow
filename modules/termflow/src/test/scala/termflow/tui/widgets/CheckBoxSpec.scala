package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class CheckBoxSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private def textOf(node: VNode): String = node match
    case TextNode(_, _, runs) => runs.map(_.txt).mkString
    case _                    => fail(s"expected TextNode, got $node")

  test("checked Unicode renders ☒ and the label") {
    val cb = CheckBox(label = "Autosave", checked = true)
    val s  = textOf(cb)
    assert(s.contains("☒"))
    assert(s.contains("Autosave"))
  }

  test("unchecked Unicode renders ☐ and the label") {
    val cb = CheckBox(label = "Autosave", checked = false)
    val s  = textOf(cb)
    assert(s.contains("☐"))
  }

  test("ASCII fallback renders [x] / [ ]") {
    val on  = CheckBox("On", checked = true, unicode = false)
    val off = CheckBox("Off", checked = false, unicode = false)
    assert(textOf(on).contains("[x]"))
    assert(textOf(off).contains("[ ]"))
  }

  test("focused checkbox bolds its label") {
    val cb = CheckBox("X", checked = false, focused = true)
    cb match
      case TextNode(_, _, runs) =>
        // The label run (after the marker + space) should be bold.
        assert(runs.last.style.bold, s"focused label should be bold, got ${runs.last.style}")
      case other => fail(s"unexpected node: $other")
  }

  test("unfocused checkbox does not bold its label") {
    val cb = CheckBox("X", checked = false, focused = false)
    cb match
      case TextNode(_, _, runs) => assert(!runs.last.style.bold)
      case other                => fail(s"unexpected node: $other")
  }

  test("CheckBox.glyph maps every (checked, unicode) combo") {
    assert(CheckBox.glyph(checked = true) == "☒")
    assert(CheckBox.glyph(checked = false) == "☐")
    assert(CheckBox.glyph(checked = true, unicode = false) == "[x]")
    assert(CheckBox.glyph(checked = false, unicode = false) == "[ ]")
  }

  test("CheckBox.width accounts for the marker glyph plus space plus label") {
    assert(CheckBox.width("hi") == 1 + 1 + 2)                  // ☒ + ' ' + 'hi'
    assert(CheckBox.width("hi", unicode = false) == 3 + 1 + 2) // [x] + ' ' + 'hi'
  }

  test("CheckBox is positioned at the supplied coordinate") {
    val cb = CheckBox("x", checked = false, at = Coord(10.x, 5.y))
    cb match
      case TextNode(x, y, _) =>
        assert(x.value == 10)
        assert(y.value == 5)
      case other => fail(s"unexpected node: $other")
  }
