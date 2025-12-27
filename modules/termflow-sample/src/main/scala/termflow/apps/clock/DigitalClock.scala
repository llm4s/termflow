package termflow.apps.clock

import termflow.tui._
import termflow.tui.TuiPrelude._
import termflow.tui.Color.{ Blue, Red }

import java.time.LocalTime

object DigitalClock {

  final case class SubSource[T](sub: Sub[Msg], value: T)

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    clock: SubSource[String],
    messages: List[String],
    error: Option[String],
    input: Sub[Msg],
    prompt: Prompt.State
  )

  // --- Messages ---
  sealed trait Msg
  object Msg {
    case object Tick                                           extends Msg
    case object StartClock                                     extends Msg
    case object StopClock                                      extends Msg
    final case class AddMessage(input: String)                 extends Msg
    case object Exit                                           extends Msg
    final case class ConsoleInputKey(key: KeyDecoder.InputKey) extends Msg
    final case class ConsoleInputError(error: Throwable)       extends Msg
  }

  import Msg._

  object App extends TuiApp[Model, Msg] {
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        SubSource[String](
          Sub.Every(1000, () => Tick, ctx),
          LocalTime.now().toString
        ),
        List.empty,
        None,
        Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        Prompt.State()
      )

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match {
        case Tick =>
          m.copy(clock = m.clock.copy(value = LocalTime.now().toString))

        case StartClock =>
          if (m.clock.sub.isActive)
            m.copy(error = Some("⏱️ Clock already running"))
          else
            m.copy(clock = m.clock.copy(sub = Sub.Every(1000, () => Tick, ctx)), error = None)

        case StopClock =>
          m.clock.sub.cancel()
          m.copy(clock = m.clock.copy(sub = Sub.NoSub), messages = "🛑 Clock stopped" :: m.messages)

        case AddMessage(input) =>
          m.copy(messages = s"💬 You said: $input" :: m.messages)

        case Exit =>
          Tui(m, Cmd.Exit)

        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toMsg)
          maybeCmd match {
            case Some(cmd) => Tui(m.copy(prompt = nextPrompt), cmd)
            case None      => m.copy(prompt = nextPrompt)
          }

        case ConsoleInputError(e) =>
          m.copy(messages = m.messages :+ s"Console Input Error: ${e.getMessage}")
      }

    override def view(m: Model): RootNode = {
      val prefix         = "[]> "
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
      val boxWidth       = math.max(40, m.terminalWidth - 4)
      RootNode(
        m.terminalWidth,
        10,
        children = List(
          BoxNode(
            1.x,
            1.y,
            boxWidth,
            5 + m.messages.length + 4,
            children = List(),
            style = Style(border = true, fg = Blue)
          ),
          TextNode(2.x, 2.y, List(s"🕒 Time: ${m.clock.value}".text)),
          TextNode(2.x, 3.y, List(s"──────────────────────────────".text(fg = Red)))
        ) ++ m.messages.zipWithIndex.map { case (msg, idx) => TextNode(2.x, (4 + idx).y, List(msg.text)) } ++ List(
          TextNode(2.x, (4 + m.messages.length).y, List(s"──────────────────────────────".text(fg = Blue))),
          TextNode(2.x, (5 + m.messages.length).y, List(s"Commands:".text)),
          TextNode(2.x, (6 + m.messages.length).y, List(s"  stopclock → stop ticking".text)),
          TextNode(2.x, (7 + m.messages.length).y, List(s"  startclock→ start ticking".text)),
          TextNode(2.x, (8 + m.messages.length).y, List(s"  exit      → quit".text))
        ),
        input = Some(
          InputNode(
            2.x,
            (9 + m.messages.length).y,
            renderedPrompt.text,
            Style(),
            cursor = renderedPrompt.cursorIndex
          )
        )
      )
    }

    override def toMsg(input: PromptLine): Result[Msg] =
      input.trim.toLowerCase match {
        case "startclock" => Right(StartClock)
        case "stopclock"  => Right(StopClock)
        case "exit"       => Right(Exit)
        case other        => Right(AddMessage(other))
      }
  }
}
