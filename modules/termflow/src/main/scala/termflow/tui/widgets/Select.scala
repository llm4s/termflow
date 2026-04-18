package termflow.tui.widgets

import termflow.tui.*

/**
 * Dropdown picker — a closed "current value" box that expands into a
 * [[ListView]] to pick a new value.
 *
 * Built on top of [[ListView]]: the open state delegates keystrokes and
 * selection rendering to ListView; the closed state shows the currently
 * selected value and can be opened again.
 *
 * {{{
 * import termflow.tui.*
 * import termflow.tui.widgets.*
 *
 * given Theme = Theme.dark
 *
 * case class Model(role: Select.State[String], ...)
 *
 * val initial = Select.State.of(Vector("Admin", "Editor", "Viewer"))
 *
 * // update:
 * case Msg.ConsoleInputKey(k) if model.fm.isFocused(RoleId) =>
 *   val (next, maybe) = Select.handleKey(model.role, k)(Some(_))
 *   model.copy(role = next).tui
 *
 * // view:
 * Select.view(model.role, lineWidth = 24, focused = model.fm.isFocused(RoleId))
 * }}}
 *
 * ## Open vs closed layout
 *
 * When closed the widget renders a single row that looks like `▾ Admin  ` —
 * one cell tall, easy to slot into a form. When open, the current row
 * stays anchored at the top with a `▴` indicator and the available
 * options list beneath it (`1 + visibleRows` cells tall). Callers whose
 * layout needs a stable height should size the surrounding container to
 * `Select.maxHeight(visibleRows)` rather than `1`.
 */
object Select:

  /**
   * State for a single `Select`.
   *
   * Wraps a [[ListView.State]] so the dropdown's scrolling + selection
   * logic doesn't have to be duplicated. The `open` flag toggles the
   * expanded vs collapsed look; `listState.selected` is the value shown
   * in the closed form.
   *
   * @param listState   Underlying list model. `selected` is the current value.
   * @param open        Whether the dropdown is expanded.
   * @param visibleRows Viewport height when open. Stored on `listState`.
   */
  final case class State[A](
    listState: ListView.State[A],
    open: Boolean
  ):

    /** Items backing the dropdown. */
    def items: Vector[A] = listState.items

    /** Current selected value, if any. */
    def value: Option[A] = listState.selectedItem

    /** `true` when there are no choices available. */
    def isEmpty: Boolean = listState.isEmpty

    /** Number of choices. */
    def size: Int = listState.size

    /** Explicitly open / close the dropdown. */
    def opened: State[A]  = copy(open = true)
    def closed: State[A]  = copy(open = false)
    def toggled: State[A] = copy(open = !open)

    /** Replace the choices collection; delegates to [[ListView.State.withItems]]. */
    def withItems(items: Vector[A]): State[A] =
      copy(listState = listState.withItems(items))

    /** Set the selected value by index (clamped). Keeps `open` unchanged. */
    def selectIndex(idx: Int): State[A] =
      if isEmpty then this
      else
        val clamped = math.max(0, math.min(size - 1, idx))
        copy(listState = listState.copy(selected = clamped))

  object State:

    /** Empty dropdown — open will display "(none)" until withItems is called. */
    def empty[A](visibleRows: Int = 5): State[A] =
      State(ListView.State.empty[A](visibleRows), open = false)

    /** Dropdown populated with `items`; selection parks at the first item. */
    def of[A](items: Vector[A], visibleRows: Int = 5): State[A] =
      State(ListView.State.of(items, visibleRows), open = false)

  /**
   * Process one keystroke.
   *
   * Behaviour depends on `state.open`:
   *
   * Closed:
   *   - `Enter` / `Space` / `ArrowDown` → open the dropdown; no message
   *   - other keys                       → unchanged
   *
   * Open:
   *   - `Escape`                         → close without changing selection
   *   - `Enter` / `Space`                → commit the ListView's current
   *                                        selection, close, and dispatch
   *                                        `onChange(value)`
   *   - `ArrowUp` / `ArrowDown` / `Home` / `End` → delegate to ListView
   *   - other keys                       → unchanged
   *
   * The open-Enter path is the standard "I've picked a value" gesture.
   * Closed-Enter opens the dropdown (mirror of a desktop combobox).
   */
  def handleKey[A, Msg](
    state: State[A],
    key: KeyDecoder.InputKey
  )(onChange: A => Option[Msg]): (State[A], Option[Msg]) =
    import KeyDecoder.InputKey.*
    if state.open then
      key match
        case Escape =>
          (state.closed, None)
        case Enter | CharKey(' ') =>
          val committed = state.closed
          (committed, committed.value.flatMap(onChange))
        case _ =>
          val (nextList, _) = ListView.handleKey(state.listState, key)(_ => None)
          (state.copy(listState = nextList), None)
    else
      key match
        case Enter | CharKey(' ') | ArrowDown =>
          if state.isEmpty then (state, None) else (state.opened, None)
        case _ =>
          (state, None)

  /**
   * Render the dropdown as a single `VNode` — a composed [[BoxNode]] whose
   * rendered height depends on `state.open`:
   *
   *   - closed: 1 row, `▾ <value>` in `theme.primary` when focused else `theme.foreground`
   *   - open:   1 row (`▴ <value>`) plus `visibleRows` option rows below
   *
   * @param state     Select state.
   * @param at        Top-left cell. Defaults to `(1, 1)`.
   * @param lineWidth Total width in cells.
   * @param focused   Whether the dropdown has focus.
   * @param render    Formatter for each item. Defaults to `_.toString`.
   */
  def view[A](
    state: State[A],
    at: Coord = Coord(XCoord(1), YCoord(1)),
    lineWidth: Int = 24,
    focused: Boolean = false,
    render: A => String = (a: A) => a.toString
  )(using theme: Theme): VNode =
    val width = math.max(3, lineWidth)

    val indicator = if state.open then "▴ " else "▾ "
    val label: String =
      if state.isEmpty then "(none)"
      else state.value.map(render).getOrElse("")

    val headerStyle =
      if focused then Style(fg = theme.background, bg = theme.primary)
      else Style(fg = theme.foreground)

    val headerContent = indicator + label
    val headerTxt     = headerContent.take(width).padTo(width, ' ')
    val headerRow     = TextNode(at.x, at.y, List(Text(headerTxt, headerStyle)))

    if !state.open then BoxNode(at.x, at.y, width, 1, children = List(headerRow), style = Style())
    else
      // Underlying ListView rendered right below the header.
      val listNode = ListView.view(
        state.listState,
        at = Coord(at.x, at.y + 1),
        lineWidth = width,
        focused = focused,
        render = render
      )
      val totalHeight = 1 + state.listState.visibleRows
      BoxNode(at.x, at.y, width, totalHeight, children = List(headerRow, listNode), style = Style())

  /** Width of a rendered Select. Mirrors [[view]]. */
  def width(lineWidth: Int): Int = math.max(3, lineWidth)

  /** Maximum height the widget may occupy when open (useful for sizing parent containers). */
  def maxHeight(visibleRows: Int): Int = 1 + math.max(1, visibleRows)
