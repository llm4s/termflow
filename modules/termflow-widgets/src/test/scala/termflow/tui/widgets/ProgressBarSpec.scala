package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*

class ProgressBarSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private def rowChars(vnode: VNode, totalWidth: Int): String =
    val root  = RootNode(totalWidth, 1, List(vnode), None)
    val frame = AnsiRenderer.buildFrame(root)
    (0 until frame.width).map(c => frame.cells(0)(c).ch).mkString

  test("ProgressBar.renderedWidth accounts for the percent suffix"):
    assert(ProgressBar.renderedWidth(10, showPercent = true) == 15)
    assert(ProgressBar.renderedWidth(10, showPercent = false) == 10)

  test("zero progress produces all empty cells"):
    val v   = ProgressBar(value = 0.0, width = 5, showPercent = false)
    val row = rowChars(v, 5)
    assert(row == "░░░░░")

  test("full progress produces all filled cells"):
    val v   = ProgressBar(value = 1.0, width = 5, showPercent = false)
    val row = rowChars(v, 5)
    assert(row == "█████")

  test("mid progress splits the bar proportionally"):
    val v   = ProgressBar(value = 0.5, width = 4, showPercent = false)
    val row = rowChars(v, 4)
    assert(row.count(_ == '█') == 2)
    assert(row.count(_ == '░') == 2)

  test("values outside [0, 1] are clamped"):
    val neg = ProgressBar(value = -0.2, width = 4, showPercent = false)
    assert(rowChars(neg, 4) == "░░░░")
    val big = ProgressBar(value = 2.0, width = 4, showPercent = false)
    assert(rowChars(big, 4) == "████")

  test("filled cells use theme.primary and empty cells use theme.foreground"):
    val v     = ProgressBar(value = 0.5, width = 4, showPercent = false)
    val cells = AnsiRenderer.buildFrame(RootNode(4, 1, List(v), None)).cells
    assert(cells(0)(0).style.fg == Theme.dark.primary)
    assert(cells(0)(1).style.fg == Theme.dark.primary)
    assert(cells(0)(2).style.fg == Theme.dark.foreground)
    assert(cells(0)(3).style.fg == Theme.dark.foreground)

  test("percent suffix is rendered when showPercent=true"):
    val v   = ProgressBar(value = 0.25, width = 4, showPercent = true)
    val row = rowChars(v, ProgressBar.renderedWidth(4))
    // 25% → 1 filled, 3 empty, plus " 25%"
    assert(row == "█░░░  25%")

  test("ProgressBar composes inside Layout.row with measured width"):
    val bar = ProgressBar(value = 0.5, width = 10, showPercent = true)
    val lay = Layout.row(gap = 0)(bar)
    assert(lay.measure == (15, 1))
