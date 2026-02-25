package termflow.apps.counter

import termflow.tui._
import termflow.tui.TuiPrelude._
import termflow.tui.Color.{ Blue, Green, Yellow }
import termflow.TimeFormatter

import scala.concurrent.ExecutionContext
import scala.util.Try

object FutureCounter {
  implicit val ec: ExecutionContext = ExecutionContext.global

  def main(args: Array[String]): Unit = {
    val _ = args
    TuiRuntime.run(App)
  }

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    count: Counter,
    staus: String,
    input: Sub[Msg],
    prompt: Prompt.State,
    spinner: Sub[Msg],
    spinnerIndex: Int
  )

  sealed trait Msg
  object Msg {
    case object Increment                                      extends Msg
    case object Decrement                                      extends Msg
    final case class UpdateWith(counter: Counter)              extends Msg
    final case class Busy(action: String)                      extends Msg
    case object SpinnerTick                                    extends Msg
    final case class ConsoleInputKey(key: KeyDecoder.InputKey) extends Msg
    final case class ConsoleInputError(error: Throwable)       extends Msg
  }

  import Msg._

  object App extends TuiApp[Model, Msg] {
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        count = Counter(0),
        staus = s"init::${TimeFormatter.getCurrentTime}",
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State(),
        spinner = Sub.NoSub,
        spinnerIndex = 0
      )

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match {
        case Increment =>
          Tui(
            m,
            Cmd.FCmd(
              m.count.asyncIncrement()(ec),
              (c: Counter) => Cmd.GCmd(UpdateWith(c)),
              onEnqueue = Some(Busy(s"InCrementING::${TimeFormatter.getCurrentTime}"))
            )
          )
        case Decrement =>
          Tui(
            m,
            Cmd.FCmd(
              m.count.asyncDecrement()(ec),
              (c: Counter) => Cmd.GCmd(UpdateWith(c)),
              onEnqueue = Some(Busy(s"DeCrementING::${TimeFormatter.getCurrentTime}"))
            )
          )
        case UpdateWith(c) =>
          // stop spinner when work completes
          if (m.spinner.isActive) m.spinner.cancel()
          m.copy(count = c, staus = s"done::${TimeFormatter.getCurrentTime}", spinner = Sub.NoSub, spinnerIndex = 0)
        case Busy(action) =>
          // start spinner if not already active
          if (m.spinner.isActive) m.copy(staus = action)
          else m.copy(staus = action, spinner = Sub.Every(200, () => SpinnerTick, ctx))
        case SpinnerTick =>
          m.copy(spinnerIndex = (m.spinnerIndex + 1) % 4)
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
      val boxWidth       = math.max(2, m.terminalWidth - 4)
      RootNode(
        m.terminalWidth,
        14,
        children = List(
          BoxNode(1.x, 1.y, boxWidth, 7, children = List(), style = Style(border = true, fg = Blue)),
          TextNode(
            2.x,
            2.y,
            List(
              s"Current count: ".text,
              Text(s"${m.count.count}", renderCount(m.count.count))
            )
          ),
          TextNode(
            2.x,
            3.y,
            List(
              Text(statusWithSpinner(m), Style(fg = Green))
            )
          ),
          TextNode(2.x, 5.y, List("Commands:".text(fg = Yellow))),
          TextNode(2.x, 6.y, List("  increment | +".text)),
          TextNode(2.x, 7.y, List("  decrement | -".text))
        ),
        input = Some(
          InputNode(2.x, 11.y, renderedPrompt.text, Style(), cursor = renderedPrompt.cursorIndex)
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

    private def statusWithSpinner(m: Model): String = {
      val frames = Array("|", "/", "-", "\\")
      if (m.spinner.isActive) s"${m.staus} ${frames(m.spinnerIndex % frames.length)}" else m.staus
    }
  }
}
