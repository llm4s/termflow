package termflow.apps.clock

import termflow.tui.Color.Blue
import termflow.tui.Color.Red
import termflow.tui.RandomUtil.RandomSourceAtFixedRate
import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

import java.time.LocalTime
import scala.util.Random

object DigitalClockWithRandomSource:

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
  enum Msg:
    case Tick
    case RandomValue(value: Int)
    case StartRandom
    case StopRandom
    case StopClock
    case AddMessage(input: String)
    case Exit
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)

  import Msg._

  // --- Application Definition ---
  object App extends TuiApp[Model, Msg]:
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
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Tick =>
          m.copy(clock = m.clock.copy(value = LocalTime.now().toString)).tui

        case RandomValue(v) =>
          m.copy(random = m.random.copy(value = v)).tui

        case StartRandom =>
          if m.random.sub.isActive then
            m.copy(error = Some("You  have attempted to start random generator while it is already running")).tui
          else
            m.copy(
              random = m.random.copy(
                sub =
                  new RandomSourceAtFixedRate[Int, Msg](1700, () => Random.nextInt(5000), (v: Int) => RandomValue(v))
                    .asEventSource(ctx)
              )
            ).tui

        case StopRandom =>
          m.random.sub.cancel()
          m.copy(random = m.random.copy(sub = Sub.NoSub), messages = "🛑 Random Generator Stopped" :: m.messages).tui

        case StopClock =>
          m.clock.sub.cancel()
          m.copy(clock = m.clock.copy(sub = Sub.NoSub), messages = "🛑 Clock stopped" :: m.messages).tui

        case AddMessage(input) =>
          val updatedMsgs = s"💬 You said: $input" :: m.messages
          m.copy(messages = updatedMsgs).tui

        case Exit =>
          Tui(m, Cmd.Exit)

        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(m.copy(prompt = nextPrompt), cmd)
            case None      => m.copy(prompt = nextPrompt).tui
        case ConsoleInputError(e) =>
          m.copy(messages = m.messages :+ s"Console Input Error: ${e.getMessage}").tui

    override def view(m: Model): RootNode =
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
          TextNode(2.x, 4.y, List("──────────────────────────────".text(fg = Red)))
        ) ++ m.messages.zipWithIndex.map { case (msg, idx) => TextNode(2.x, (5 + idx).y, List(msg.text)) } ++ List(
          TextNode(2.x, (5 + m.messages.length).y, List("──────────────────────────────".text(fg = Blue))),
          TextNode(2.x, (5 + m.messages.length + 1).y, List("Commands:".text)),
          TextNode(2.x, (5 + m.messages.length + 2).y, List("  start    → start random numbers".text)),
          TextNode(2.x, (5 + m.messages.length + 3).y, List("  stop     → stop random numbers".text)),
          TextNode(2.x, (5 + m.messages.length + 4).y, List("  stopclock→ stop ticking".text)),
          TextNode(2.x, (5 + m.messages.length + 5).y, List("  exit     → quit".text))
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

    override def toMsg(input: PromptLine): Result[Msg] =
      input.value.trim.toLowerCase match
        case "start"     => Right(StartRandom)
        case "stop"      => Right(StopRandom)
        case "stopclock" => Right(StopClock)
        case "exit"      => Right(Exit)
        case other       => Right(AddMessage(other))
