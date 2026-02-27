package termflow.apps.echo

import termflow.tui.Color._
import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

object EchoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  // === Model ===
  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    messages: List[String],
    input: Sub[Msg],
    prompt: Prompt.State
  )

  enum Msg:
    case AddMessage(input: String)
    case Clear
    case Exit
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)

  import Msg._

  // === App ===
  object App extends TuiApp[Model, Msg]:
    private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

    private def contentWidth(terminalWidth: Int): Int =
      math.max(8, terminalWidth - 10)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        messages = List.empty,
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State()
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncTerminalSize(m, ctx)
      msg match
        case AddMessage(input) =>
          // Wrap long input into lines that fit in the box
          val wrappedLines = wrapText(input, contentWidth(sized.terminalWidth) - 4)
          val newMsgs      = sized.messages ++ wrappedLines
          sized.copy(messages = newMsgs.takeRight(30)).tui

        case Clear =>
          sized.copy(messages = Nil).tui

        case Exit =>
          Tui(sized, Cmd.Exit)

        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](sized.prompt, k)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(sized.copy(prompt = nextPrompt), cmd)
            case None      => sized.copy(prompt = nextPrompt).tui

        case ConsoleInputError(_) =>
          sized.tui // ignore for now

    override def view(m: Model): RootNode =
      val panelTop = 1
      // Keep a compact, stable message panel and grow only when needed.
      val panelHeight    = math.max(6, math.min(16, m.messages.length + 2))
      val prefix         = "[]> "
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
      val innerHeight    = math.max(1, panelHeight - 2)
      val visible        = m.messages.takeRight(innerHeight)
      val startY         = panelTop + 1 + (innerHeight - visible.length)
      val clearRowWidth  = math.max(1, contentWidth(m.terminalWidth))

      // Explicitly clear message rows each frame so shorter content
      // cannot leave stale characters behind.
      val clearNodes =
        (0 until innerHeight).toList.map { i =>
          TextNode(2.x, (panelTop + 1 + i).y, List(Text(" " * clearRowWidth, Style())))
        }

      val messageNodes =
        if visible.nonEmpty then
          visible.zipWithIndex.map { case (msg, i) =>
            TextNode(2.x, (startY + i).y, List(msg.text))
          }
        else
          List(
            TextNode(2.x, (panelTop + 1 + (innerHeight / 2)).y, List("Type a message and press Enter".text(fg = Cyan)))
          )

      RootNode(
        width = m.terminalWidth,
        height = panelHeight + 8,
        children = List(
          TextNode(2.x, panelTop.y, List(Text("Messages", Style(fg = Blue, underline = true))))
        ) ++ clearNodes ++ messageNodes ++
          List(
            TextNode(2.x, (panelTop + panelHeight).y, List("──────────────────────────────".text(fg = Cyan))),
            TextNode(2.x, (panelTop + panelHeight + 1).y, List("Commands:".text(fg = Yellow))),
            TextNode(2.x, (panelTop + panelHeight + 2).y, List("  /clear -> clear chat".text)),
            TextNode(2.x, (panelTop + panelHeight + 3).y, List("  exit   -> quit".text))
          ),
        input = Some(
          InputNode(
            2.x,
            (panelTop + panelHeight + 5).y,
            prompt = renderedPrompt.text,
            style = Style(fg = Green),
            cursor = renderedPrompt.cursorIndex
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      input.value.trim match
        case ""       => Left(termflow.tui.TermFlowError.Validation("Empty input"))
        case "/clear" => Right(Clear)
        case "exit"   => Right(Exit)
        case other    => Right(AddMessage(other))

    // === Helpers ===
    private def wrapText(text: String, maxWidth: Int): List[String] =
      text.split("\n").toList.flatMap { line =>
        if line.length <= maxWidth then List(line)
        else line.grouped(maxWidth).toList
      }
