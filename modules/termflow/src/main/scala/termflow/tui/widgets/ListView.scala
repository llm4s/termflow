package termflow.tui.widgets

import termflow.tui.*

/**
 * Scrollable list of items with a selection cursor.
 *
 * Renamed from `List` to avoid shadowing `scala.List` inside callers.
 *
 * Like every stateful widget in this package, `ListView` is purely
 * value-based: state lives in the app's model, `handleKey` advances it
 * without side effects, and `view` renders a tree of `VNode`s.
 *
 * ## Anatomy
 *
 *   - [[ListView.State]] — `(items, selected, scrollOffset, visibleRows)`
 *   - [[ListView.handleKey]] — pure function: `(state, key) → (state, Option[Msg])`
 *   - [[ListView.view]] — produces a [[BoxNode]] of exactly `visibleRows` cells tall
 *
 * ## Usage
 *
 * {{{
 * import termflow.tui.*
 * import termflow.tui.widgets.*
 *
 * given Theme = Theme.dark
 * val ListId  = FocusId("tasks")
 *
 * case class Model(
 *   tasks: ListView.State[String],
 *   fm:    FocusManager
 * )
 *
 * // In update:
 * case Msg.ConsoleInputKey(k) if model.fm.isFocused(ListId) =>
 *   val (next, maybe) = ListView.handleKey(model.tasks, k)(item => Some(Msg.Open(item)))
 *   val nextModel = model.copy(tasks = next)
 *   maybe.fold(nextModel.tui)(m => Tui(nextModel, Cmd.GCmd(m)))
 *
 * // In view:
 * ListView.view(model.tasks, lineWidth = 30, focused = model.fm.isFocused(ListId))
 * }}}
 *
 * ## Selection rendering
 *
 * Focus gates the cursor:
 *   - **focused + selected**: row painted in inverse video
 *     (`bg = theme.primary, fg = theme.background`) with a `▸ ` prefix
 *   - **focused, non-selected**: `  ` (two-space) prefix, `theme.foreground`
 *   - **unfocused**: every row shows `  ` and `theme.foreground`, no
 *     visible selection mark — consistent with Button which shows no
 *     focused-state visuals when it isn't actually focused
 *
 * The two-cell prefix is preserved across all rows to keep column
 * alignment stable when focus arrives or leaves.
 */
object ListView:

  /**
   * State for a single list.
   *
   * Invariants maintained by [[handleKey]]:
   *   - `selected` is in `[0, items.size)` when non-empty, or `0` when empty
   *   - `scrollOffset` is in `[0, max(0, items.size - visibleRows)]`
   *   - `selected` is always inside the visible window
   *     `[scrollOffset, scrollOffset + visibleRows)`
   *
   * `visibleRows` is the viewport height in cells, not the number of items.
   *
   * @param items        The full collection, not just the visible slice.
   * @param selected     Index of the currently-highlighted item.
   * @param scrollOffset Index of the topmost visible item.
   * @param visibleRows  Number of rows rendered at a time (minimum 1).
   */
  final case class State[A](
    items: Vector[A],
    selected: Int,
    scrollOffset: Int,
    visibleRows: Int
  ):

    /** `true` when the list has no items. */
    def isEmpty: Boolean = items.isEmpty

    /** Total number of items. */
    def size: Int = items.size

    /** The currently highlighted item, if any. */
    def selectedItem: Option[A] = items.lift(selected)

    /** Replace the items collection; re-clamps `selected` / `scrollOffset`. */
    def withItems(newItems: Vector[A]): State[A] =
      if newItems.isEmpty then copy(items = Vector.empty, selected = 0, scrollOffset = 0)
      else
        val sel    = math.max(0, math.min(newItems.size - 1, selected))
        val vis    = math.max(1, visibleRows)
        val maxOff = math.max(0, newItems.size - vis)
        val off    = math.max(0, math.min(maxOff, scrollOffset))
        // Ensure selected is visible.
        val fixedOff =
          if sel < off then sel
          else if sel >= off + vis then sel - vis + 1
          else off
        copy(items = newItems, selected = sel, scrollOffset = math.max(0, math.min(maxOff, fixedOff)))

  object State:

    /** Empty list with the given viewport height. */
    def empty[A](visibleRows: Int = 5): State[A] =
      State(Vector.empty[A], 0, 0, math.max(1, visibleRows))

    /** List populated with `items`; selection parks at the first item. */
    def of[A](items: Vector[A], visibleRows: Int = 5): State[A] =
      State(items, 0, 0, math.max(1, visibleRows))

  /**
   * Process one keystroke, returning a new state and optionally a message
   * (typically when the user presses Enter on the highlighted row).
   *
   * Behaviour:
   *   - `ArrowUp` / `ArrowDown`     move selection by 1, clamped
   *   - `Home` / `End`              jump to first / last item
   *   - `Enter` / `Space`           call `onSelect(selectedItem)` if any
   *   - other keys                  no change
   *
   * Selection movements also scroll the viewport to keep the selection
   * visible. Pass-through for unrecognised keys so the caller can layer
   * on additional shortcuts (e.g. a page-down) via composition.
   */
  def handleKey[A, Msg](
    state: State[A],
    key: KeyDecoder.InputKey
  )(onSelect: A => Option[Msg]): (State[A], Option[Msg]) =
    import KeyDecoder.InputKey.*
    if state.isEmpty then (state, None)
    else
      key match
        case ArrowUp              => (moveTo(state, state.selected - 1), None)
        case ArrowDown            => (moveTo(state, state.selected + 1), None)
        case Home                 => (moveTo(state, 0), None)
        case End                  => (moveTo(state, state.size - 1), None)
        case Enter | CharKey(' ') => (state, state.selectedItem.flatMap(onSelect))
        case _                    => (state, None)

  /** Move selection to `target` (clamped) and adjust scroll offset so it stays visible. */
  private def moveTo[A](state: State[A], target: Int): State[A] =
    val vis     = math.max(1, state.visibleRows)
    val clamped = math.max(0, math.min(state.size - 1, target))
    val off =
      if clamped < state.scrollOffset then clamped
      else if clamped >= state.scrollOffset + vis then clamped - vis + 1
      else state.scrollOffset
    state.copy(selected = clamped, scrollOffset = math.max(0, off))

  /**
   * Render the list as a `BoxNode` (no border) exactly `visibleRows` tall
   * and `lineWidth` wide.
   *
   * @param state     List state.
   * @param at        Top-left cell. Defaults to `(1, 1)` for layout composition.
   * @param lineWidth Row width in cells.
   * @param focused   Whether the list currently has focus.
   * @param render    How to format one item. Defaults to `_.toString`.
   */
  def view[A](
    state: State[A],
    at: Coord = Coord(XCoord(1), YCoord(1)),
    lineWidth: Int = 30,
    focused: Boolean = false,
    render: A => String = (a: A) => a.toString
  )(using theme: Theme): VNode =
    val width = math.max(3, lineWidth) // need room for "▸ " + at least 1 char
    val vis   = math.max(1, state.visibleRows)

    val children = (0 until vis).map { row =>
      val itemIdx = state.scrollOffset + row
      val rowY    = at.y + row
      if itemIdx >= state.size then
        // Blank filler row (erases any prior content when the list shrinks).
        TextNode(at.x, rowY, List(Text(" " * width, Style())))
      else
        val isSelected = itemIdx == state.selected
        val content    = render(state.items(itemIdx))
        val truncated  = content.take(width - 2)
        val padded     = truncated.padTo(width - 2, ' ')
        val showCursor = isSelected && focused
        val prefix     = if showCursor then "▸ " else "  "
        val style =
          if showCursor then Style(fg = theme.background, bg = theme.primary)
          else Style(fg = theme.foreground)
        TextNode(
          at.x,
          rowY,
          List(Text(prefix, style), Text(padded, style))
        )
    }.toList

    BoxNode(at.x, at.y, width, vis, children = children, style = Style())

  /** Width of a rendered list with the given `lineWidth`. Mirrors [[view]]. */
  def width(lineWidth: Int): Int = math.max(3, lineWidth)
