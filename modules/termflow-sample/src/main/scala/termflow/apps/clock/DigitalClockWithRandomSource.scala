package termflow.apps.clock

import termflow.tui._
import termflow.tui.TuiPrelude._
import termflow.tui.RandomUtil.RandomSourceAtFixedRate
import termflow.tui.Color.{ Blue, Red }

import java.time.LocalTime
import scala.concurrent.ExecutionContext
import scala.util.Random

object DigitalClockWithRandomSource {

  implicit val ec: ExecutionContext = ExecutionContext.global

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  final case class SubSource[T](sub: Sub[Msg], value: T)

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    clock: SubSource[String],
    random: SubSource[Int],
    messages: List[String],
    error: Option[String],
    input: Sub[Msg],
    prompt: Prompt.State
  )

  // --- Messages (Events) ---
  sealed trait Msg
  object Msg {
    case object Tick                                           extends Msg
    final case class RandomValue(value: Int)                   extends Msg
    case object StartRandom                                    extends Msg
    case object StopRandom                                     extends Msg
    case object StopClock                                      extends Msg
    final case class AddMessage(input: String)                 extends Msg
    case object Exit                                           extends Msg
    final case class ConsoleInputKey(key: KeyDecoder.InputKey) extends Msg
    final case class ConsoleInputError(error: Throwable)       extends Msg
  }

  import Msg._

  // --- Application Definition ---
  object App extends TuiApp[Model, Msg] {
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        SubSource[String](
          Sub.Every(1000, () => Tick, ctx),
          LocalTime.now().toString
        ),
        SubSource[Int](
          new RandomSourceAtFixedRate[Int, Msg](1700, () => Random.nextInt(5000), (v: Int) => RandomValue(v))
            .asEventSource(ctx),
          0
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

        case RandomValue(v) =>
          m.copy(random = m.random.copy(value = v))

        case StartRandom =>
          if (m.random.sub.isActive)
            m.copy(error = Some("You  have attempted to start random generator while it is already running"))
          else
            m.copy(
              random = m.random.copy(
                sub =
                  new RandomSourceAtFixedRate[Int, Msg](1700, () => Random.nextInt(5000), (v: Int) => RandomValue(v))
                    .asEventSource(ctx)
              )
            )

        case StopRandom =>
          m.random.sub.cancel()
          m.copy(random = m.random.copy(sub = Sub.NoSub), messages = "🛑 Random Generator Stopped" :: m.messages)

        case StopClock =>
          m.clock.sub.cancel()
          m.copy(clock = m.clock.copy(sub = Sub.NoSub), messages = "🛑 Clock stopped" :: m.messages)

        case AddMessage(input) =>
          val updatedMsgs = s"💬 You said: $input" :: m.messages
          m.copy(messages = updatedMsgs)

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
      // Use terminal width with a small right margin.
      val boxWidth = math.max(40, m.terminalWidth - 4)
      RootNode(
        m.terminalWidth,
        10,
        children = List(
          BoxNode(
            1.x,
            1.y,
            boxWidth,
            5 + m.messages.length + 6,
            children = List(),
            style = Style(border = true, fg = Blue)
          ),
          TextNode(2.x, 2.y, List(s"🕒 Time: ${m.clock.value}".text)),
          TextNode(2.x, 3.y, List(s"🎲 Random: ${m.random.value}".text)),
          TextNode(2.x, 4.y, List(s"──────────────────────────────".text(fg = Red)))
        ) ++ m.messages.zipWithIndex.map { case (msg, idx) => TextNode(2.x, (5 + idx).y, List(msg.text)) } ++ List(
          TextNode(2.x, (5 + m.messages.length).y, List(s"──────────────────────────────".text(fg = Blue))),
          TextNode(2.x, (5 + m.messages.length + 1).y, List(s"Commands:".text)),
          TextNode(2.x, (5 + m.messages.length + 2).y, List(s"  start    → start random numbers".text)),
          TextNode(2.x, (5 + m.messages.length + 3).y, List(s"  stop     → stop random numbers".text)),
          TextNode(2.x, (5 + m.messages.length + 4).y, List(s"  stopclock→ stop ticking".text)),
          TextNode(2.x, (5 + m.messages.length + 5).y, List(s"  exit     → quit".text))
        ),
        input = Some(
          InputNode(
            2.x,
            (4 + m.messages.length + 8).y,
            renderedPrompt.text,
            Style(),
            cursor = renderedPrompt.cursorIndex
          )
        )
      )
    }

    override def toMsg(input: PromptLine): Result[Msg] =
      input.trim.toLowerCase match {
        case "start"     => Right(StartRandom)
        case "stop"      => Right(StopRandom)
        case "stopclock" => Right(StopClock)
        case "exit"      => Right(Exit)
        case other       => Right(AddMessage(other))
      }
  }
}
