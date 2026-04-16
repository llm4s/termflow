package termflow.tui.widgets

import termflow.tui.*

/**
 * Single-line editable text field — the first stateful widget in the
 * library and the building block for forms.
 *
 * `TextField` is **purely value-based**: state lives in the app's model,
 * `handleKey` advances it, and `view` renders it. There is no implicit
 * cursor placement: the cursor in a focused field is drawn as an
 * inverse-video cell so multiple fields can be on screen simultaneously
 * without competing for the OS cursor (which can only land in one place,
 * via `RootNode.input`).
 *
 * For single-prompt apps that want the OS cursor (typing into a shell-like
 * line at the bottom of the screen) the existing `Prompt` infrastructure
 * is still the right choice. `TextField` is for forms where multiple
 * editable fields live alongside other widgets.
 *
 * ## Anatomy
 *
 *   - [[TextField.State]] — `(buffer, cursor, placeholder)` triple
 *   - [[TextField.handleKey]] — pure function: `(state, key) → (state, Option[Msg])`
 *   - [[TextField.view]] — produces a `TextNode` of exactly `lineWidth` cells
 *
 * Unlike `Prompt.handleKey`, this one returns a plain `Option[Msg]` (not
 * `Option[Cmd[Msg]]`) and does not hard-wire any global keys: `Ctrl+C` /
 * `Ctrl+D` are not handled here. Apps should route those via [[Keymap]].
 *
 * ## Usage with FocusManager + Keymap
 *
 * {{{
 * import termflow.tui.*
 * import termflow.tui.widgets.*
 *
 * given Theme = Theme.dark
 * val NameId  = FocusId("name")
 * val EmailId = FocusId("email")
 *
 * case class Model(
 *   name: TextField.State,
 *   email: TextField.State,
 *   fm: FocusManager
 * )
 *
 * enum Msg:
 *   case Submit(name: String)
 *   case ConsoleInputKey(k: KeyDecoder.InputKey)
 *
 * // In update:
 * case Msg.ConsoleInputKey(k) if model.fm.isFocused(NameId) =>
 *   val (next, maybe) = TextField.handleKey(model.name, k)(s => Some(Msg.Submit(s)))
 *   val nextModel = model.copy(name = next)
 *   maybe.fold(nextModel.tui)(m => Tui(nextModel, Cmd.GCmd(m)))
 *
 * // In view:
 * Layout.column(gap = 1)(
 *   TextField.view(model.name,  lineWidth = 30, focused = model.fm.isFocused(NameId)),
 *   TextField.view(model.email, lineWidth = 30, focused = model.fm.isFocused(EmailId))
 * )
 * }}}
 */
object TextField:

  /**
   * Editable state for a single field.
   *
   * @param buffer      Current text content.
   * @param cursor      Insertion-point index in `[0, buffer.length]`.
   * @param placeholder Hint text shown when `buffer` is empty AND the field
   *                    is not focused. Empty disables the placeholder.
   */
  final case class State(
    buffer: String = "",
    cursor: Int = 0,
    placeholder: String = ""
  ):

    /** `true` when the buffer has no characters. */
    def isEmpty: Boolean = buffer.isEmpty

    /** Number of characters in the buffer (not counting the placeholder). */
    def length: Int = buffer.length

  object State:

    /** Empty field with no placeholder. */
    val empty: State = State()

    /** Empty field with a placeholder shown when unfocused. */
    def withPlaceholder(p: String): State = State(placeholder = p)

    /** Pre-populated field with the cursor at the end. */
    def of(text: String): State = State(buffer = text, cursor = text.length)

  /**
   * Process one keystroke, returning a new state and optionally a message
   * to dispatch (typically the submitted value on `Enter`).
   *
   * Behaviour:
   *   - `CharKey(c)`  insert at cursor
   *   - `Backspace`   remove the char before cursor
   *   - `Delete`      remove the char at cursor
   *   - `ArrowLeft`   cursor -= 1 (clamped)
   *   - `ArrowRight`  cursor += 1 (clamped)
   *   - `Home`        cursor = 0
   *   - `End`         cursor = buffer length
   *   - `Enter`       call `onSubmit(buffer)`, then clear buffer + cursor
   *   - other keys    no change
   *
   * `Ctrl+C` / `Ctrl+D` and other framework-level keys are intentionally
   * **not** handled — apps should route those via a top-level [[Keymap]]
   * before falling through to per-field key dispatch.
   *
   * @param state    Current field state.
   * @param key      Decoded key event.
   * @param onSubmit Callback invoked on `Enter` with the submitted buffer.
   *                 Return `Some(msg)` to dispatch a message, `None` to
   *                 silently accept the input.
   */
  def handleKey[Msg](
    state: State,
    key: KeyDecoder.InputKey
  )(onSubmit: String => Option[Msg]): (State, Option[Msg]) =
    import KeyDecoder.InputKey.*
    key match
      case Enter =>
        val msg = onSubmit(state.buffer)
        (state.copy(buffer = "", cursor = 0), msg)

      case Backspace =>
        if state.cursor > 0 then
          val nb = state.buffer.take(state.cursor - 1) + state.buffer.drop(state.cursor)
          (state.copy(buffer = nb, cursor = state.cursor - 1), None)
        else (state, None)

      case Delete =>
        if state.cursor < state.buffer.length then
          val nb = state.buffer.take(state.cursor) + state.buffer.drop(state.cursor + 1)
          (state.copy(buffer = nb), None)
        else (state, None)

      case CharKey(ch) =>
        val nb = state.buffer.take(state.cursor) + ch + state.buffer.drop(state.cursor)
        (state.copy(buffer = nb, cursor = state.cursor + 1), None)

      case ArrowLeft =>
        (state.copy(cursor = math.max(0, state.cursor - 1)), None)

      case ArrowRight =>
        (state.copy(cursor = math.min(state.buffer.length, state.cursor + 1)), None)

      case Home => (state.copy(cursor = 0), None)
      case End  => (state.copy(cursor = state.buffer.length), None)

      case _ => (state, None)

  /**
   * Render the field as a single-row [[TextNode]] of exactly `lineWidth`
   * cells.
   *
   * Visual rules:
   *   - **focused**: text in `theme.primary`, the cell at `cursor` painted
   *     in inverse video (`bg = theme.primary, fg = theme.background`)
   *     so users can see where they're editing without needing the OS
   *     cursor. Placeholder is hidden on focus.
   *   - **unfocused**: text in `theme.foreground`. If buffer is empty and a
   *     placeholder was set, the placeholder is shown (no inverse cell).
   *
   * The buffer is truncated to `lineWidth` cells. If the cursor sits past
   * `lineWidth - 1` it's drawn at the rightmost cell — basic "scroll-to-end"
   * behaviour. A future PR can add a viewport that scrolls horizontally;
   * for v1, prefer choosing `lineWidth` ≥ the realistic input length.
   *
   * @param state     Field state.
   * @param at        Top-left cell. Defaults to `(1, 1)` for layout composition.
   * @param lineWidth Cell-width of the rendered field.
   * @param focused   Whether this field currently has focus.
   */
  def view(
    state: State,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    lineWidth: Int = 20,
    focused: Boolean = false
  )(using theme: Theme): VNode =
    val width           = math.max(1, lineWidth)
    val showPlaceholder = state.buffer.isEmpty && !focused && state.placeholder.nonEmpty
    val display         = if showPlaceholder then state.placeholder else state.buffer
    val padded          = display.take(width).padTo(width, ' ')

    if focused then
      val cIdx        = math.min(width - 1, math.max(0, state.cursor))
      val before      = padded.take(cIdx)
      val cursorChar  = padded.charAt(cIdx).toString
      val after       = padded.drop(cIdx + 1)
      val baseStyle   = Style(fg = theme.primary)
      val cursorStyle = Style(fg = theme.background, bg = theme.primary)
      TextNode(
        at.x,
        at.y,
        List(
          Text(before, baseStyle),
          Text(cursorChar, cursorStyle),
          Text(after, baseStyle)
        )
      )
    else
      val style =
        if showPlaceholder then Style(fg = theme.foreground)
        else Style(fg = theme.foreground)
      TextNode(at.x, at.y, List(Text(padded, style)))

  /** Width of a `TextField` rendered with the given `lineWidth`. */
  def width(lineWidth: Int): Int = math.max(1, lineWidth)
