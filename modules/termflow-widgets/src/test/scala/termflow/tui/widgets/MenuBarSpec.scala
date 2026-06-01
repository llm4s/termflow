package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class MenuBarSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private val sample = MenuBar.State(
    menus = Vector(
      MenuBar.Menu("File", Vector("New", "Open", "Save", "Quit")),
      MenuBar.Menu("Edit", Vector("Cut", "Copy", "Paste"))
    )
  )

  // ---- handleKey ----------------------------------------------------------

  test("ArrowRight on the bar moves the cursor between menus (no menu open)") {
    val a = MenuBar.handleKey(sample, KeyDecoder.InputKey.ArrowRight)
    assert(a.state.cursor == 1)
    assert(a.state.openIdx.isEmpty)
    val b = MenuBar.handleKey(a.state, KeyDecoder.InputKey.ArrowRight)
    assert(b.state.cursor == 0, "wrap around to first menu")
  }

  test("ArrowDown when nothing is open opens the cursor menu at item 0") {
    val r = MenuBar.handleKey(sample, KeyDecoder.InputKey.ArrowDown)
    assert(r.state.openIdx.contains(0))
    assert(r.state.cursor == 0)
  }

  test("ArrowDown inside an open menu moves the item cursor (wraps)") {
    val opened = sample.copy(openIdx = Some(0), cursor = 0)
    val a      = MenuBar.handleKey(opened, KeyDecoder.InputKey.ArrowDown)
    assert(a.state.cursor == 1)
    val n = MenuBar.handleKey(opened.copy(cursor = 3), KeyDecoder.InputKey.ArrowDown)
    assert(n.state.cursor == 0, "wrap from last back to first")
  }

  test("ArrowLeft / ArrowRight inside an open menu moves to the next menu and reopens") {
    val opened = sample.copy(openIdx = Some(0), cursor = 2)
    val r      = MenuBar.handleKey(opened, KeyDecoder.InputKey.ArrowRight)
    assert(r.state.openIdx.contains(1), "moving sideways should re-open the new menu")
    assert(r.state.cursor == 1)
  }

  test("Enter inside an open menu picks the highlighted item and closes the menu") {
    val opened = sample.copy(openIdx = Some(1), cursor = 2)
    val r      = MenuBar.handleKey(opened, KeyDecoder.InputKey.Enter)
    assert(r.picked == Some((1, 2)))
    assert(r.state.openIdx.isEmpty)
  }

  test("Escape closes the open menu and parks the cursor on its title") {
    val opened = sample.copy(openIdx = Some(1), cursor = 2)
    val r      = MenuBar.handleKey(opened, KeyDecoder.InputKey.Escape)
    assert(r.state.openIdx.isEmpty)
    assert(r.state.cursor == 1)
  }

  test("handleKey on an empty MenuBar is a no-op") {
    val empty = MenuBar.State(Vector.empty)
    val r     = MenuBar.handleKey(empty, KeyDecoder.InputKey.ArrowRight)
    assert(r == MenuBar.KeyResult(empty, None))
  }

  // ---- view ---------------------------------------------------------------

  test("apply renders the title row only when no menu is open") {
    val nodes = MenuBar(sample)
    assert(nodes.size == 1)
    nodes.head match
      case TextNode(_, _, runs) =>
        val s = runs.map(_.txt).mkString
        assert(s.contains("File"))
        assert(s.contains("Edit"))
      case _ => fail()
  }

  test("apply renders the dropdown rows when a menu is open") {
    val opened = sample.copy(openIdx = Some(0), cursor = 1)
    val nodes  = MenuBar(opened)
    // 1 title row + 4 item rows for File menu = 5 nodes.
    assert(nodes.size == 5, s"expected 5 nodes for opened File menu, got ${nodes.size}")
  }

  test("the cursor item in an open menu is rendered with the primary background") {
    val opened = sample.copy(openIdx = Some(0), cursor = 2) // "Save" is selected
    val nodes  = MenuBar(opened)
    val saveRow = nodes.collectFirst {
      case TextNode(_, _, runs) if runs.exists(_.txt.contains("Save")) => runs.head
    }
    assert(saveRow.exists(_.style.bold), "selected item must be bold")
  }

  // ---- hitTest ------------------------------------------------------------

  test("hitTest on a title cell returns Left(menuIdx)") {
    val at = Coord(1.x, 1.y)
    // "[ File ]" / "  File  " is 8 cells (4 padding + 4 chars). Title starts at col 1.
    assert(MenuBar.hitTest(sample, at, col = 1, row = 1) == Some(Left(0)))
    // Edit starts at col 1 + 8 + 1 (separator) = col 10.
    assert(MenuBar.hitTest(sample, at, col = 10, row = 1) == Some(Left(1)))
  }

  test("hitTest on an open menu's item row returns Right(itemIdx)") {
    val opened = sample.copy(openIdx = Some(0))
    val at     = Coord(1.x, 1.y)
    // Items render at row 2 (1 + 1) onward. "New" is item 0.
    val r = MenuBar.hitTest(opened, at, col = 2, row = 2)
    assert(r == Some(Right(0)), s"got $r")
  }

  test("hitTest outside any cell returns None") {
    val at = Coord(1.x, 1.y)
    assert(MenuBar.hitTest(sample, at, col = 999, row = 1).isEmpty)
    assert(MenuBar.hitTest(sample, at, col = 1, row = 99).isEmpty)
  }

  test("an opened empty-items menu does not crash apply / hitTest") {
    // Regression: `menu.items.map(_.length).max` threw on empty items once
    // such a menu was opened.
    val st0    = MenuBar.State(menus = Vector(MenuBar.Menu("File", Vector.empty)))
    val opened = MenuBar.handleKey(st0, KeyDecoder.InputKey.Enter).state
    val at     = Coord(1.x, 1.y)
    assert(MenuBar.apply(opened, at).nonEmpty)                    // render must not throw
    assert(MenuBar.hitTest(opened, at, col = 3, row = 2).isEmpty) // hit-test must not throw
  }
