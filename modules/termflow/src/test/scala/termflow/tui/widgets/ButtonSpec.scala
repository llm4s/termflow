package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class ButtonSpec extends AnyFunSuite:

  given Theme = Theme.dark

  test("Button.width reports label length plus padding"):
    assert(Button.width("OK") == 6)
    assert(Button.width("") == 4)

  test("unfocused Button renders [ label ] in the theme foreground"):
    val v     = Button("OK", focused = false)
    val cells = AnsiRenderer.buildFrame(RootNode(Button.width("OK"), 1, List(v), None)).cells
    val row   = (0 until 6).map(c => cells(0)(c).ch).mkString
    assert(row == "[ OK ]")
    // Label cell uses theme.foreground, not primary, when unfocused.
    assert(cells(0)(2).style.fg == Theme.dark.foreground)
    assert(!cells(0)(2).style.bold)

  test("focused Button paints the label bold and in theme.primary"):
    val v     = Button("Save", focused = true)
    val cells = AnsiRenderer.buildFrame(RootNode(Button.width("Save"), 1, List(v), None)).cells
    // Cell at column 3 (1-based) holds the 'S' of "Save"
    val labelCell = cells(0)(2)
    assert(labelCell.ch == 'S')
    assert(labelCell.style.fg == Theme.dark.primary)
    assert(labelCell.style.bold)

  test("Button composes inside Layout.row with the natural width"):
    val buttons = Layout.row(gap = 1)(
      Button("A", focused = true),
      Button("BB", focused = false)
    )
    // widths: 5 + 1 + 6 = 12
    assert(buttons.measure == (12, 1))
    val placed = buttons.resolve(Coord(1.x, 1.y))
    assert(placed.size == 2)
    assert(placed(0).x.value == 1)
    assert(placed(1).x.value == 7) // "[ A ]" is 5 wide, + 1 gap, so next at 7
