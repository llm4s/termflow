package termflow.apps.multiline

import termflow.tui.*
import termflow.tui.Color.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets

/**
 * Demo: multiline prompt using [[widgets.MultiLineInput]].
 *
 * Key bindings:
 *   - Type anything, Enter inserts a newline
 *   - Ctrl+D submits the message
 *   - ESC quits
 */
object MultilinePromptDemo:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  // === Model ===

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    messages: Vector[String],
    editor: widgets.MultiLineInput.State,
    input: Sub[Msg]
  )

  enum Msg:
    case Submit(text: String)
    case EditorKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)
    case Quit

  import Msg.*

  // === App ===

  object App extends TuiApp[Model, Msg]:

    private def syncSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        messages = Vector.empty,
        editor = widgets.MultiLineInput.State.empty,
        input = Sub.InputKey(
          key => EditorKey(key),
          err => ConsoleInputError(err),
          ctx
        )
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncSize(m, ctx)
      msg match
        case Submit(text) =>
          sized.copy(messages = sized.messages :+ text).tui

        case EditorKey(k) =>
          k match
            case KeyDecoder.InputKey.Escape =>
              Tui(sized, Cmd.Exit)
            case _ =>
              val submitKey = KeyDecoder.InputKey.Ctrl('D')
              val (nextEditor, maybeCmd) =
                widgets.MultiLineInput.handleKey[Msg](sized.editor, k, submitKey)(text => Right(Msg.Submit(text)))
              maybeCmd match
                case Some(cmd) =>
                  Tui(sized.copy(editor = nextEditor), cmd)
                case None =>
                  sized.copy(editor = nextEditor).tui

        case Quit =>
          Tui(sized, Cmd.Exit)

        case ConsoleInputError(_) =>
          sized.tui

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation("This demo uses MultiLineInput, not single-line Prompt"))

    override def view(m: Model): RootNode =
      given Theme = Theme.dark

      val w = m.terminalWidth
      val h = m.terminalHeight

      val editorHeight = 5
      val fullWidth    = math.min(w, 80)
      val msgWidth     = math.max(20, fullWidth - 4)

      // --- Top status bar ---
      val status =
        TextNode(
          1.x,
          1.y,
          List(
            Text(" Multiline Prompt Demo ", Style(fg = Black, bg = Cyan, bold = true)),
            Text("  ESC: quit  |  Ctrl+D: submit  |  Enter: newline ", Style(fg = White, bg = Magenta))
          )
        )

      // --- Messages header ---
      val msgHeaderY = 3
      val msgHeader  = TextNode(2.x, msgHeaderY.y, List(Text("Messages:", Style(fg = Yellow, underline = true))))

      // --- Messages body ---
      val msgStartY = msgHeaderY + 1
      val msgEndY   = h - editorHeight - 3

      // Flatten all messages into display lines, most recent last
      val allLines: Vector[String] = m.messages.flatMap(_.split("\n", -1).toVector)

      val msgVisible   = math.max(0, msgEndY - msgStartY)
      val visibleLines = allLines.takeRight(msgVisible)
      val padCount     = msgVisible - visibleLines.size

      val msgNodes = (0 until msgVisible).map { i =>
        val y = msgStartY + i
        if i < padCount then TextNode(2.x, y.y, List(Text(" " * msgWidth, Style())))
        else
          val text = visibleLines(i - padCount)
          val display =
            if text.length <= msgWidth then text
            else text.take(msgWidth - 3) + "..."
          TextNode(2.x, y.y, List(Text(s" $display", Style(fg = White))))
      }.toList

      // --- Editor area (bottom) ---
      val editorLabelY = msgEndY + 1
      val editorLabel = TextNode(
        2.x,
        editorLabelY.y,
        List(Text("Message (Ctrl+D to send, Enter for newline):", Style(fg = Yellow)))
      )

      val editorY     = editorLabelY + 1
      val editorWidth = math.max(20, fullWidth - 4)

      val editorNodes = widgets.MultiLineInput.render(
        state = m.editor,
        width = editorWidth,
        height = editorHeight,
        at = Coord(2.x, editorY.y)
      )

      // --- Footer help ---
      val footerY = editorY + editorHeight
      val footer  = TextNode(2.x, footerY.y, List(Text("─" * fullWidth, Style(fg = Cyan))))

      RootNode(
        width = w,
        height = h,
        input = None,
        children = List(status, msgHeader) ++ msgNodes ++ List(editorLabel) ++ editorNodes ++ List(footer)
      )
