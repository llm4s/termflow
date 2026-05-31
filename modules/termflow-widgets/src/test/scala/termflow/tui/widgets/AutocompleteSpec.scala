package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*

class AutocompleteSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private val initial = Autocomplete.State.of(Vector("apple", "banana", "cherry", "blueberry"))

  // ---- filtered + selected ------------------------------------------------

  test("empty query returns all options in order") {
    assert(initial.filtered == Vector("apple", "banana", "cherry", "blueberry"))
  }

  test("filtered uses case-insensitive contains by default") {
    val withQuery = initial.copy(input = Prompt.State("BAN".toVector, cursor = 3))
    assert(withQuery.filtered == Vector("banana"))
  }

  test("custom matches predicate is honoured") {
    val state = initial.copy(
      input = Prompt.State("a".toVector, cursor = 1),
      matches = (q, a) => a.startsWith(q)
    )
    assert(state.filtered == Vector("apple"), s"prefix-match should pick only apple, got ${state.filtered}")
  }

  test("clamped resets selectedIdx into the visible range") {
    val state   = initial.copy(input = Prompt.State("ber".toVector, cursor = 3), selectedIdx = 99)
    val clamped = state.clamped
    assert(clamped.selectedIdx == 0, "out-of-range index clamped down")
    assert(clamped.selected == Some("blueberry"))
  }

  // ---- handleKey ----------------------------------------------------------

  test("typing a printable key narrows the filter and resets selectedIdx") {
    val r = Autocomplete.handleKey(initial.copy(selectedIdx = 3), KeyDecoder.InputKey.CharKey('b'))
    assert(r.state.query == "b")
    assert(r.state.selectedIdx == 0, "filter change must reset cursor to top")
    assert(r.state.filtered == Vector("banana", "blueberry"))
  }

  test("ArrowDown moves the selection (wraps)") {
    val r1 = Autocomplete.handleKey(initial, KeyDecoder.InputKey.ArrowDown)
    assert(r1.state.selectedIdx == 1)
    // From idx 3 (last) wraps to 0.
    val r2 = Autocomplete.handleKey(initial.copy(selectedIdx = 3), KeyDecoder.InputKey.ArrowDown)
    assert(r2.state.selectedIdx == 0)
  }

  test("ArrowUp wraps to the bottom from idx 0") {
    val r = Autocomplete.handleKey(initial, KeyDecoder.InputKey.ArrowUp)
    assert(r.state.selectedIdx == 3)
  }

  test("Enter returns picked = Some(currentSelected)") {
    val r = Autocomplete.handleKey(initial.copy(selectedIdx = 2), KeyDecoder.InputKey.Enter)
    assert(r.picked == Some("cherry"))
  }

  test("Enter when filter has no matches returns picked = None") {
    val empty = initial.copy(input = Prompt.State("xyz".toVector, cursor = 3))
    val r     = Autocomplete.handleKey(empty, KeyDecoder.InputKey.Enter)
    assert(r.picked.isEmpty)
  }

  test("Backspace narrows the input through Prompt and re-clamps the selection") {
    val state = initial.copy(input = Prompt.State("be".toVector, cursor = 2), selectedIdx = 0)
    val r     = Autocomplete.handleKey(state, KeyDecoder.InputKey.Backspace)
    assert(r.state.query == "b")
    assert(r.state.selectedIdx == 0)
  }

  // ---- view ---------------------------------------------------------------

  test("view emits an InputNode + one row per visible filtered option") {
    val nodes = Autocomplete.view(initial, width = 16, maxVisible = 6)
    assert(nodes.head.isInstanceOf[VNode.InputNode], "first node must be the InputNode")
    assert(nodes.tail.size == 4, "all 4 items fit under maxVisible = 6")
  }

  test("view truncates the dropdown to maxVisible rows") {
    val many  = Autocomplete.State.of((1 to 20).map(i => s"item-$i").toVector)
    val nodes = Autocomplete.view(many, width = 12, maxVisible = 5)
    assert(nodes.tail.size == 5)
  }

  test("Autocomplete.height = 1 + min(maxVisible, filtered.size)") {
    assert(Autocomplete.height(initial, maxVisible = 6) == 1 + 4)
    assert(Autocomplete.height(initial, maxVisible = 2) == 1 + 2)
    val empty = initial.copy(input = Prompt.State("xyz".toVector, cursor = 3))
    assert(Autocomplete.height(empty, maxVisible = 6) == 1)
  }

  test("view scrolls the dropdown so the cursor row stays visible") {
    val many     = Autocomplete.State.of((1 to 20).map(i => s"item-$i").toVector).copy(selectedIdx = 12)
    val nodes    = Autocomplete.view(many, width = 16, maxVisible = 5)
    val rowTexts = nodes.tail.map { case n: VNode.TextNode => n.txt.map(_.txt).mkString; case _ => "" }
    // Cursor at 12 with maxVisible=5 should anchor so item-13 (index 12) is the last visible row.
    assert(rowTexts.exists(_.contains("item-13")), s"expected 'item-13' visible — got $rowTexts")
    // ...and items before the window should NOT appear.
    assert(!rowTexts.exists(_.contains("item-1 ")), s"items above the scroll should not render — got $rowTexts")
  }

  test("view drops the primary-bar highlight when not focused") {
    val state   = initial.copy(selectedIdx = 1) // some non-zero selection
    val focused = Autocomplete.view(state, width = 16, maxVisible = 6, focused = true)
    val blurred = Autocomplete.view(state, width = 16, maxVisible = 6, focused = false)
    def rowStyle(nodes: List[VNode], i: Int): Style = nodes.tail(i) match
      case t: VNode.TextNode => t.txt.head.style
      case _                 => fail("expected TextNode")
    // Focused selection has a primary background; unfocused only has a primary fg.
    assert(rowStyle(focused, 1).bg != Color.Default, "focused row should have a bg")
    assert(rowStyle(blurred, 1).bg == Color.Default, "unfocused row should drop the bg bar")
  }
