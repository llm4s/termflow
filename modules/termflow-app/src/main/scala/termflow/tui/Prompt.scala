package termflow.tui

import termflow.tui.KeyDecoder.InputKey
import termflow.tui.TuiPrelude.*

object Prompt:

  final case class State(buffer: Vector[Char] = Vector.empty, cursor: Int = 0)

  def render(state: State): String =
    state.buffer.mkString

  final case class RenderedLine(text: String, cursorIndex: Int, prefixLength: Int = 0)

  /**
   * Visual column the cursor occupies inside `state.buffer`, computed
   * via [[WCWidth.stringWidth]] across the characters before the
   * cursor. Wide characters (CJK, fullwidth, most emoji) contribute 2
   * columns; combining marks contribute 0; everything else is 1.
   *
   * The renderer already uses this math when positioning the hardware
   * cursor on an `InputNode`; [[cursorColumn]] is exposed for apps that
   * need the same number for their own layout (e.g. drawing a custom
   * caret indicator next to the buffer).
   */
  def cursorColumn(state: State): Int =
    val cursor = math.max(0, math.min(state.buffer.length, state.cursor))
    val text   = state.buffer.take(cursor).mkString
    WCWidth.stringWidth(text)

  /** Render the prompt buffer with a fixed prefix, returning full text and cursor index. */
  def renderWithPrefix(state: State, prefix: String): RenderedLine =
    val content = render(state)
    RenderedLine(
      text = prefix + content,
      cursorIndex = prefix.length + state.cursor,
      prefixLength = prefix.length
    )

  private def normalized(state: State): State =
    val len     = state.buffer.length
    val clamped = math.max(0, math.min(len, state.cursor))
    if clamped == state.cursor then state else state.copy(cursor = clamped)

  def handleKey[G](state: State, k: InputKey)(toMsg: PromptLine => Result[G]): (State, Option[Cmd[G]]) =
    k match
      // Clean exit via Ctrl+C or Ctrl+D
      case KeyDecoder.InputKey.Ctrl('C') | KeyDecoder.InputKey.Ctrl('D') =>
        (State(Vector.empty, 0), Some(Cmd.Exit))

      case KeyDecoder.InputKey.Enter =>
        val raw = render(state)
        val cmd = toMsg(PromptLine(raw)).fold(err => Cmd.TermFlowErrorCmd(err), g => Cmd.GCmd(g))
        (State(Vector.empty, 0), Some(cmd))

      case KeyDecoder.InputKey.Backspace =>
        if state.cursor > 0 then
          // Step back by a grapheme so combining marks, surrogate pairs, and
          // ZWJ sequences delete as one unit.
          val text    = state.buffer.mkString
          val prevIdx = Grapheme.previousBoundary(text, state.cursor)
          val deleteN = state.cursor - prevIdx
          val newBuf  = state.buffer.patch(prevIdx, Nil, deleteN)
          (normalized(state.copy(buffer = newBuf, cursor = prevIdx)), None)
        else (state, None)

      case KeyDecoder.InputKey.Delete =>
        if state.cursor < state.buffer.length then
          val text    = state.buffer.mkString
          val nextIdx = Grapheme.nextBoundary(text, state.cursor)
          val deleteN = nextIdx - state.cursor
          val newBuf  = state.buffer.patch(state.cursor, Nil, deleteN)
          (normalized(state.copy(buffer = newBuf)), None)
        else (state, None)

      case KeyDecoder.InputKey.CharKey(ch) =>
        val newBuf = state.buffer.patch(state.cursor, Seq(ch), 0)
        (normalized(state.copy(buffer = newBuf, cursor = state.cursor + 1)), None)

      case KeyDecoder.InputKey.Supplementary(cp) =>
        val chars = Character.toChars(cp)
        val newBuf = state.buffer.patch(state.cursor, chars.toIndexedSeq, 0)
        (normalized(state.copy(buffer = newBuf, cursor = state.cursor + chars.length)), None)

      case KeyDecoder.InputKey.ArrowLeft =>
        // Grapheme-aware step so the cursor doesn't strand combining marks.
        val text    = state.buffer.mkString
        val prevIdx = Grapheme.previousBoundary(text, state.cursor)
        (normalized(state.copy(cursor = prevIdx)), None)

      case KeyDecoder.InputKey.ArrowRight =>
        val text    = state.buffer.mkString
        val nextIdx = Grapheme.nextBoundary(text, state.cursor)
        (normalized(state.copy(cursor = nextIdx)), None)

      case KeyDecoder.InputKey.Home =>
        (normalized(state.copy(cursor = 0)), None)

      case KeyDecoder.InputKey.End =>
        (normalized(state.copy(cursor = state.buffer.length)), None)

      case _ =>
        (state, None)
