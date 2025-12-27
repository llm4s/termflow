package termflow.apps.counter

import termflow.tui.Color.Blue
import termflow.tui._
import termflow.tui.TuiPrelude._

import scala.util.Try

object SyncCounter {

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    counter: Counter,
    input: Sub[Msg],
    prompt: Prompt.State
  )

  sealed trait Msg
  object Msg {
    case object Increment                                      extends Msg
    case object Decrement                                      extends Msg
    final case class ConsoleInputKey(key: KeyDecoder.InputKey) extends Msg
    final case class ConsoleInputError(error: Throwable)       extends Msg
  }

  import Msg._

  object App extends TuiApp[Model, Msg] {
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        counter = Counter(0),
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State()
      )

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match {
        case Increment =>
          m.copy(counter = m.counter.syncIncrement())
        case Decrement =>
          m.copy(counter = m.counter.syncDecrement())
        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toMsg)
          maybeCmd match {
            case Some(cmd) => Tui(m.copy(prompt = nextPrompt), cmd)
            case None      => m.copy(prompt = nextPrompt)
          }
        case ConsoleInputError(_) => m
      }

    override def view(m: Model): RootNode = {
      val prefix         = "[]> "
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
      // Use terminal width with a small right margin.
      val boxWidth = math.max(40, m.terminalWidth - 4)
      RootNode(
        m.terminalWidth,
        12,
        children = List(
          BoxNode(1.x, 1.y, boxWidth, 6, children = List(), style = Style(border = true, fg = Blue)),
          TextNode(
            2.x,
            2.y,
            List(
              s"Current count: ".text,
              Text(s"${m.counter.count}", renderCount(m.counter.count))
            )
          ),
          TextNode(2.x, 4.y, List("Commands:".text(fg = Color.Yellow))),
          TextNode(2.x, 5.y, List("  increment | +".text)),
          TextNode(2.x, 6.y, List("  decrement | -".text))
        ),
        input = Some(
          InputNode(2.x, 9.y, renderedPrompt.text, Style(), cursor = renderedPrompt.cursorIndex)
        )
      )
    }

    def renderCount(c: Int): Style =
      if (c == 0) Style(fg = Color.Black)
      else if (c < 0) Style(fg = Color.Red)
      else Style(fg = Color.Green)

    override def toMsg(input: PromptLine): Result[Msg] =
      Try {
        input.trim.toLowerCase match {
          case "increment" | "+" =>
            Increment
          case "decrement" | "-" =>
            Decrement
        }
      }.toEither.left.map(e => TermFlowError.Unexpected(e.getMessage))
  }
}
