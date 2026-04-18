package termflow.apps.echo

import termflow.tui.*
import termflow.tui.Color.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

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
      val clearRowWidth  = math.max(1, contentWidth(m.terminalWidth))

      // The message panel is always exactly `innerHeight` rows. We pad with
      // blank-text rows above the messages so they bottom-align inside the
      // panel — the explicit blanks (rather than `Layout.Spacer`) double as
      // erase rows so shorter content doesn't ghost previous frames.
      def blankRow: Layout =
        Layout.Elem(TextNode(1.x, 1.y, List(Text(" " * clearRowWidth, Style()))))

      val panelRows: List[Layout] =
        if visible.nonEmpty then
          val padding = innerHeight - visible.length
          List.fill(padding)(blankRow) ++
            visible.map(msg => Layout.Elem(TextNode(1.x, 1.y, List(msg.text))))
        else
          val before = innerHeight / 2
          val after  = innerHeight - before - 1
          List.fill(before)(blankRow) ++
            List(Layout.Elem(TextNode(1.x, 1.y, List("Type a message and press Enter".text(fg = Cyan))))) ++
            List.fill(after)(blankRow)

      val column = Layout.Column(
        gap = 0,
        children = List(
          Layout.Elem(TextNode(1.x, 1.y, List(Text("Messages", Style(fg = Blue, underline = true)))))
        ) ++ panelRows ++ List(
          // Original layout reserves one blank row between the panel
          // (which is `innerHeight` tall) and the separator (which sits at
          // `panelTop + panelHeight = panelTop + innerHeight + 2`).
          Layout.Spacer(1, 1),
          Layout.Elem(TextNode(1.x, 1.y, List("──────────────────────────────".text(fg = Cyan)))),
          Layout.Elem(TextNode(1.x, 1.y, List("Commands:".text(fg = Yellow)))),
          Layout.Elem(TextNode(1.x, 1.y, List("  clear | /clear -> clear chat".text))),
          Layout.Elem(TextNode(1.x, 1.y, List("  exit           -> quit".text)))
        )
      )
      val children = column.resolve(Coord(2.x, panelTop.y))

      RootNode(
        width = m.terminalWidth,
        height = panelHeight + 8,
        children = children,
        input = Some(
          InputNode(
            2.x,
            (panelTop + panelHeight + 5).y,
            prompt = renderedPrompt.text,
            style = Style(fg = Green),
            cursor = renderedPrompt.cursorIndex,
            prefixLength = renderedPrompt.prefixLength
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      input.value.trim.toLowerCase match
        case ""       => Left(termflow.tui.TermFlowError.Validation("Empty input"))
        case "clear"  => Right(Clear)
        case "/clear" => Right(Clear)
        case "exit"   => Right(Exit)
        case other    => Right(AddMessage(other))

    // === Helpers ===
    private def wrapText(text: String, maxWidth: Int): List[String] =
      text.split("\n").toList.flatMap { line =>
        if line.length <= maxWidth then List(line)
        else line.grouped(maxWidth).toList
      }
