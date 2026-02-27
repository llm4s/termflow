package termflow.apps.counter

import termflow.tui.Color.Blue
import termflow.tui.Color.Green
import termflow.tui.Color.Yellow
import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

import scala.util.Try

object SyncCounter:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    counter: Counter,
    input: Sub[Msg],
    prompt: Prompt.State
  )

  enum Msg:
    case Increment
    case Decrement
    case Exit
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:
    private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        counter = Counter(0),
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State()
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncTerminalSize(m, ctx)
      msg match
        case Increment =>
          sized.copy(counter = sized.counter.syncIncrement()).tui
        case Decrement =>
          sized.copy(counter = sized.counter.syncDecrement()).tui
        case Exit =>
          Tui(sized, Cmd.Exit)
        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](sized.prompt, k)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(sized.copy(prompt = nextPrompt), cmd)
            case None      => sized.copy(prompt = nextPrompt).tui
        case ConsoleInputError(_) => sized.tui

    override def view(m: Model): RootNode =
      val prefix         = "[]> "
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
      // Use terminal width with a small right margin.
      val boxWidth = math.max(2, m.terminalWidth - 4)
      RootNode(
        m.terminalWidth,
        13,
        children = List(
          BoxNode(1.x, 1.y, boxWidth, 6, children = List(), style = Style(border = true, fg = Blue)),
          TextNode(
            2.x,
            2.y,
            List(
              "Current count: ".text,
              Text(s"${m.counter.count}", renderCount(m.counter.count))
            )
          ),
          TextNode(2.x, 4.y, List("Commands:".text(fg = Yellow))),
          TextNode(2.x, 5.y, List("  increment | + -> increase counter".text)),
          TextNode(2.x, 6.y, List("  decrement | - -> decrease counter".text)),
          TextNode(2.x, 7.y, List("  exit          -> quit".text))
        ),
        input = Some(
          InputNode(2.x, 10.y, renderedPrompt.text, Style(fg = Green), cursor = renderedPrompt.cursorIndex)
        )
      )

    def renderCount(c: Int): Style =
      if c == 0 then Style(fg = Color.Black)
      else if c < 0 then Style(fg = Color.Red)
      else Style(fg = Color.Green)

    override def toMsg(input: PromptLine): Result[Msg] =
      Try {
        input.value.trim.toLowerCase match
          case "increment" | "+" =>
            Increment
          case "decrement" | "-" =>
            Decrement
          case "exit" =>
            Exit
      }.toEither.left.map(e => TermFlowError.Unexpected(e.getMessage))
