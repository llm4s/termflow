package termflow.tui

import termflow.tui.KeyDecoder.InputKey
import termflow.tui.TuiPrelude._

object Prompt:

  final case class State(buffer: Vector[Char] = Vector.empty, cursor: Int = 0)

  def render(state: State): String =
    state.buffer.mkString

  final case class RenderedLine(text: String, cursorIndex: Int)

  /** Render the prompt buffer with a fixed prefix, returning full text and cursor index. */
  def renderWithPrefix(state: State, prefix: String): RenderedLine =
    val content = render(state)
    RenderedLine(
      text = prefix + content,
      cursorIndex = prefix.length + state.cursor
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

      case KeyDecoder.InputKey.Ctrl('M') | KeyDecoder.InputKey.Enter =>
        val raw = render(state)
        val cmd = toMsg(PromptLine(raw)).fold(err => Cmd.TermFlowErrorCmd(err), g => Cmd.GCmd(g))
        (State(Vector.empty, 0), Some(cmd))

      case KeyDecoder.InputKey.Backspace =>
        if state.cursor > 0 then
          val newBuf = state.buffer.patch(state.cursor - 1, Nil, 1)
          (normalized(state.copy(buffer = newBuf, cursor = state.cursor - 1)), None)
        else (state, None)

      case KeyDecoder.InputKey.Delete =>
        if state.cursor < state.buffer.length then
          val newBuf = state.buffer.patch(state.cursor, Nil, 1)
          (normalized(state.copy(buffer = newBuf)), None)
        else (state, None)

      case KeyDecoder.InputKey.CharKey(ch) =>
        val newBuf = state.buffer.patch(state.cursor, Seq(ch), 0)
        (normalized(state.copy(buffer = newBuf, cursor = state.cursor + 1)), None)

      case KeyDecoder.InputKey.ArrowLeft =>
        (normalized(state.copy(cursor = state.cursor - 1)), None)

      case KeyDecoder.InputKey.ArrowRight =>
        (normalized(state.copy(cursor = state.cursor + 1)), None)

      case KeyDecoder.InputKey.Home =>
        (normalized(state.copy(cursor = 0)), None)

      case KeyDecoder.InputKey.End =>
        (normalized(state.copy(cursor = state.buffer.length)), None)

      case _ =>
        (state, None)
