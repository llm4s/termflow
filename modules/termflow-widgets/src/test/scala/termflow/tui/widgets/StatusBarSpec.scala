package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*

class StatusBarSpec extends AnyFunSuite:

  given Theme = Theme.dark

  test("renderString places left, center, and right sections"):
    val row = StatusBar.renderString(left = "LEFT", center = "MID", right = "RIGHT", width = 20)
    assert(row.length == 20)
    assert(row.startsWith("LEFT"))
    assert(row.endsWith("RIGHT"))
    // Center 'M' lands near the middle.
    val midIdx = row.indexOf('M')
    assert(midIdx >= 6 && midIdx <= 10)

  test("center section is dropped when it won't fit"):
    // left(4) + right(5) = 9, leaves 1 cell for center — "MID"(3) doesn't fit.
    val row = StatusBar.renderString("LEFT", "MID", "RIGHT", width = 10)
    assert(row.length == 10)
    assert(row.startsWith("LEFT"))
    assert(row.endsWith("RIGHT"))
    assert(!row.contains("MID"))

  test("right section is dropped when it overlaps with left"):
    val row = StatusBar.renderString("LEFTTOOLONG", "", "RIGHT", width = 12)
    assert(row.length == 12)
    // "RIGHT" needs 5, but only 1 cell remains → dropped
    assert(!row.contains("RIGHT"))

  test("left section is truncated when wider than the total row"):
    val row = StatusBar.renderString("VERYLONGLEFT", "", "", width = 5)
    assert(row == "VERYL")

  test("zero or negative width returns an empty string"):
    assert(StatusBar.renderString("a", "b", "c", width = 0) == "")
    assert(StatusBar.renderString("a", "b", "c", width = -1) == "")

  test("StatusBar colours the row with theme.primary background"):
    val v     = StatusBar("ready", "editor", "12:34", width = 30)
    val cells = AnsiRenderer.buildFrame(RootNode(30, 1, List(v), None)).cells
    // Spot-check a cell that should be painted (the 'r' of "ready").
    val first = cells(0)(0)
    assert(first.ch == 'r')
    assert(first.style.bg == Theme.dark.primary)
    assert(first.style.fg == Theme.dark.background)

  test("StatusBar composes into a RootNode for full-frame tests"):
    val v     = StatusBar("a", "", "b", width = 5)
    val frame = AnsiRenderer.buildFrame(RootNode(5, 1, List(v), None))
    val row   = (0 until 5).map(c => frame.cells(0)(c).ch).mkString
    assert(row == "a   b")
