package termflow.tui.widgets

import termflow.tui.*

/**
 * Open ComboBox: a single-line input field with a filtered dropdown
 * underneath. The user types, the list narrows; Enter commits the
 * selected item.
 *
 * Companion to [[Select]] (closed dropdown). Where Select shows a
 * collapsed "current value" cell that opens on demand, Autocomplete
 * is always-open — typing always filters, and the selection lives in
 * the user's keystrokes.
 *
 * Filter strategy is supplied by the caller (`matches`). The default is
 * case-insensitive `contains`. Apps that want fuzzy / prefix / fuzzy-
 * weighted matching slot in their own predicate.
 *
 * State (input buffer, cursor, selected filtered index, raw option
 * list) lives in the app's model, like every other widget here. The
 * companion [[handleKey]] is a pure dispatcher that updates the state
 * — apps fold it through their own update.
 *
 * {{{
 * given Theme = Theme.dark
 * val initial = Autocomplete.State.of(Vector("apple", "banana", "cherry"))
 *
 * // update:
 * case Msg.Key(k) =>
 *   val (next, picked) = Autocomplete.handleKey(model.ac, k)
 *   model.copy(ac = next, lastPick = picked.orElse(model.lastPick)).tui
 *
 * // view:
 * Autocomplete.view(model.ac, width = 24, maxVisible = 6)
 * }}}
 */
object Autocomplete:

  /**
   * State for one Autocomplete.
   *
   * @param input        Current filter text + cursor.
   * @param options      Full unfiltered options.
   * @param selectedIdx  Index into the *filtered* list (clamped on
   *                     every state transition so it always points at
   *                     a visible row, or `0` when empty).
   * @param matches      Filter predicate. Defaults to case-insensitive
   *                     substring match on `option.toString`.
   * @param render       Display function for an option. Defaults to
   *                     `_.toString`.
   */
  final case class State[A](
    input: Prompt.State,
    options: Vector[A],
    selectedIdx: Int = 0,
    matches: (String, A) => Boolean = (q: String, a: A) => a.toString.toLowerCase.contains(q.toLowerCase),
    render: A => String = (a: A) => a.toString
  ):

    /** Current input string. */
    def query: String = input.buffer.mkString

    /** Filtered, in-order subset of `options` that matches the query. */
    def filtered: Vector[A] =
      if query.isEmpty then options
      else options.filter(a => matches(query, a))

    /** Currently-selected filtered option (None when filter has no hits). */
    def selected: Option[A] = filtered.lift(selectedIdx)

    /** Clamp the selected index into the current filtered range. */
    def clamped: State[A] =
      val f = filtered
      if f.isEmpty then copy(selectedIdx = 0)
      else copy(selectedIdx = math.max(0, math.min(f.size - 1, selectedIdx)))

  object State:
    /** Build an empty-input state over `options`. */
    def of[A](options: Vector[A]): State[A] = State(Prompt.State(), options)

  /**
   * Result of [[handleKey]]: updated state plus an optional "user
   *  picked this item" event for the app to act on.
   */
  final case class KeyResult[A](state: State[A], picked: Option[A])

  /**
   * Pure key dispatcher. Apps fold their decoded
   * `KeyDecoder.InputKey` through this. Bindings:
   *
   *   - `↑` / `↓`        — move filtered-list cursor (wraps at ends).
   *   - `Enter`          — commit the current selection. Returns
   *                        `picked = Some(item)`.
   *   - any other key    — routed through [[Prompt.handleKey]] so
   *                        printable characters edit the filter, etc.
   */
  def handleKey[A](state: State[A], key: KeyDecoder.InputKey): KeyResult[A] =
    import KeyDecoder.InputKey.*
    key match
      case ArrowDown =>
        val f = state.filtered
        if f.isEmpty then KeyResult(state, None)
        else
          val next = (state.selectedIdx + 1) % f.size
          KeyResult(state.copy(selectedIdx = next), None)

      case ArrowUp =>
        val f = state.filtered
        if f.isEmpty then KeyResult(state, None)
        else
          val next = (state.selectedIdx - 1 + f.size) % f.size
          KeyResult(state.copy(selectedIdx = next), None)

      case Enter =>
        KeyResult(state, state.selected)

      case _ =>
        // Route everything else through Prompt; reset selection to top
        // when the input changes so the user doesn't keep an old index
        // pointing into a now-empty filtered list.
        val (nextInput, _) = Prompt.handleKey[Unit](state.input, key)(_ => Right(()))
        val nextState =
          if nextInput == state.input then state
          else state.copy(input = nextInput, selectedIdx = 0)
        KeyResult(nextState.clamped, None)

  /**
   * Render the input field on the first row plus the filtered list
   * underneath (up to `maxVisible` rows). Returns:
   *
   *   - one [[InputNode]] for the prompt, plus
   *   - one [[TextNode]] per visible filtered option.
   *
   * The InputNode's `cursor` reflects the current `Prompt.State.cursor`
   * so the runtime parks the hardware cursor inside the field.
   *
   * @param state      The Autocomplete state.
   * @param width      Cell width of the input field + dropdown.
   * @param maxVisible Maximum dropdown rows. Defaults to 6.
   * @param at         Top-left of the input field row.
   * @param prefix     Optional fixed prefix (e.g. `"> "`) pinned to
   *                   the left edge of the input viewport.
   */
  def view[A](
    state: State[A],
    width: Int,
    maxVisible: Int = 6,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    prefix: String = ""
  )(using theme: Theme): List[VNode] =
    val rendered = Prompt.renderWithPrefix(state.input, prefix)
    val inputNode = VNode.InputNode(
      x = at.x,
      y = at.y,
      prompt = rendered.text,
      style = Style(fg = theme.foreground, bg = theme.background),
      cursor = rendered.cursorIndex,
      lineWidth = math.max(1, width),
      prefixLength = rendered.prefixLength
    )
    val visible = state.filtered.take(maxVisible)
    val rows = visible.zipWithIndex.toList.map { case (item, i) =>
      val isSelected = i == state.selectedIdx
      val style =
        if isSelected then Style(fg = theme.background, bg = theme.primary, bold = true)
        else Style(fg = theme.foreground)
      val marker = if isSelected then "▸ " else "  "
      val label  = state.render(item)
      val padded = s"$marker$label".take(math.max(1, width))
      TextNode(at.x, at.y + (1 + i), List(Text(padded, style)))
    }
    inputNode :: rows

  /**
   * Total cell height the widget will take given `maxVisible`. The
   *  filtered-list portion shrinks to the actual visible count.
   */
  def height(state: State[?], maxVisible: Int = 6): Int =
    1 + math.min(maxVisible, state.filtered.size)
