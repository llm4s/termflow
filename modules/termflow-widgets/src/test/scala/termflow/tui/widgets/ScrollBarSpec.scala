package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*

class ScrollBarSpec extends AnyFunSuite:

  given Theme = Theme.dark

  test("hidden when total <= visible"):
    val state = ScrollBar.State(offset = 0, visible = 10, total = 10)
    assert(!state.needed)
    assert(ScrollBar(state, length = 10).isEmpty)

  test("hidden when length <= 0"):
    val state = ScrollBar.State(offset = 0, visible = 5, total = 100)
    assert(ScrollBar(state, length = 0).isEmpty)

  test("vertical bar emits one node per cell along the length"):
    val state = ScrollBar.State(offset = 0, visible = 10, total = 100)
    val nodes = ScrollBar(state, length = 20)
    assert(nodes.size == 20)

  test("thumbRange thumb is at the top when offset = 0"):
    val state      = ScrollBar.State(offset = 0, visible = 10, total = 100)
    val (start, _) = ScrollBar.thumbRange(state, length = 20)
    assert(start == 0)

  test("thumbRange thumb is at the bottom when offset is at the max"):
    val state        = ScrollBar.State(offset = 90, visible = 10, total = 100)
    val (start, len) = ScrollBar.thumbRange(state, length = 20)
    assert(start + len == 20, s"thumb should end at the bar bottom — start=$start, len=$len, length=20")

  test("thumbRange thumb is at least one cell long"):
    val state    = ScrollBar.State(offset = 0, visible = 1, total = 1_000_000)
    val (_, len) = ScrollBar.thumbRange(state, length = 10)
    assert(len >= 1)

  test("thumbRange thumb size scales with visible/total"):
    val small     = ScrollBar.State(offset = 0, visible = 10, total = 100)
    val big       = ScrollBar.State(offset = 0, visible = 50, total = 100)
    val (_, sLen) = ScrollBar.thumbRange(small, length = 20)
    val (_, bLen) = ScrollBar.thumbRange(big, length = 20)
    assert(bLen > sLen, s"larger viewport should mean larger thumb (small=$sLen, big=$bLen)")

  test("hitTest above the thumb returns offset - visible (page up)"):
    val state  = ScrollBar.State(offset = 50, visible = 10, total = 100)
    val target = ScrollBar.hitTest(state, length = 20, cellIndex = 0)
    assert(target == 40, s"expected page-up to 40, got $target")

  test("hitTest below the thumb returns offset + visible (page down)"):
    val state  = ScrollBar.State(offset = 50, visible = 10, total = 100)
    val target = ScrollBar.hitTest(state, length = 20, cellIndex = 19)
    assert(target == 60, s"expected page-down to 60, got $target")

  test("hitTest is clamped to [0, total - visible]"):
    val low  = ScrollBar.State(offset = 0, visible = 10, total = 100)
    val high = ScrollBar.State(offset = 90, visible = 10, total = 100)
    assert(ScrollBar.hitTest(low, length = 20, cellIndex = 0) == 0)
    assert(ScrollBar.hitTest(high, length = 20, cellIndex = 19) == 90)

  test("hitTest on the thumb returns the unchanged offset"):
    val state        = ScrollBar.State(offset = 50, visible = 10, total = 100)
    val (start, len) = ScrollBar.thumbRange(state, length = 20)
    val mid          = start + len / 2
    val target       = ScrollBar.hitTest(state, length = 20, cellIndex = mid)
    assert(target == 50)

  test("offsetForDrag at cell 0 returns 0"):
    val state = ScrollBar.State(offset = 50, visible = 10, total = 100)
    assert(ScrollBar.offsetForDrag(state, length = 20, cellIndex = 0) == 0)

  test("offsetForDrag at the bottom returns the max offset"):
    val state  = ScrollBar.State(offset = 0, visible = 10, total = 100)
    val target = ScrollBar.offsetForDrag(state, length = 20, cellIndex = 19)
    assert(target == 90, s"expected end-of-track to map to 90 (total-visible), got $target")

  test("clampOffset clamps below zero and above max"):
    assert(ScrollBar.clampOffset(-5, visible = 10, total = 100) == 0)
    assert(ScrollBar.clampOffset(200, visible = 10, total = 100) == 90)

  test("State.clamped clamps the offset field"):
    val s = ScrollBar.State(offset = 200, visible = 10, total = 100).clamped
    assert(s.offset == 90)
