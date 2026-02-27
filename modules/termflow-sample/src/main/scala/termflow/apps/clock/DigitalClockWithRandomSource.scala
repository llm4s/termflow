package termflow.apps.clock

import termflow.tui.Color.Blue
import termflow.tui.Color.Green
import termflow.tui.Color.Red
import termflow.tui.Color.Yellow
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
    private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

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
      val sized = syncTerminalSize(m, ctx)
      msg match
        case Tick =>
          sized.copy(clock = sized.clock.copy(value = LocalTime.now().toString)).tui

        case RandomValue(v) =>
          sized.copy(random = sized.random.copy(value = v)).tui

        case StartRandom =>
          if sized.random.sub.isActive then
            sized.copy(error = Some("You  have attempted to start random generator while it is already running")).tui
          else
            sized
              .copy(
                random = sized.random.copy(
                  sub =
                    new RandomSourceAtFixedRate[Int, Msg](1700, () => Random.nextInt(5000), (v: Int) => RandomValue(v))
                      .asEventSource(ctx)
                )
              )
              .tui

        case StopRandom =>
          sized.random.sub.cancel()
          sized
            .copy(
              random = sized.random.copy(sub = Sub.NoSub),
              messages = "Random generator stopped" :: sized.messages
            )
            .tui

        case StopClock =>
          sized.clock.sub.cancel()
          sized.copy(clock = sized.clock.copy(sub = Sub.NoSub), messages = "Clock stopped" :: sized.messages).tui

        case AddMessage(input) =>
          val updatedMsgs = s"You said: $input" :: sized.messages
          sized.copy(messages = updatedMsgs).tui

        case Exit =>
          Tui(sized, Cmd.Exit)

        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](sized.prompt, k)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(sized.copy(prompt = nextPrompt), cmd)
            case None      => sized.copy(prompt = nextPrompt).tui
        case ConsoleInputError(e) =>
          sized.copy(messages = sized.messages :+ s"Console Input Error: ${e.getMessage}").tui

    override def view(m: Model): RootNode =
      val prefix         = "[]> "
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
      val boxWidth       = math.max(2, m.terminalWidth - 4)
      val innerWidth     = math.max(1, boxWidth - 2)
      val messageRows    = math.max(1, m.messages.length)

      def fit(line: String): String =
        if line.length <= innerWidth then line
        else if innerWidth <= 3 then line.take(innerWidth)
        else line.take(innerWidth - 3) + "..."

      val messageNodes =
        if m.messages.isEmpty then
          List(TextNode(2.x, 5.y, List(fit("Type a message and press Enter").text(fg = Green))))
        else
          m.messages.zipWithIndex.map { case (msg, idx) =>
            TextNode(2.x, (5 + idx).y, List(fit(msg).text))
          }

      val separatorY     = 5 + messageRows
      val commandsStartY = separatorY + 1
      val boxHeight      = 10 + messageRows
      val inputY         = boxHeight + 1

      RootNode(
        m.terminalWidth,
        inputY + 1,
        children = List(
          BoxNode(
            1.x,
            1.y,
            boxWidth,
            boxHeight,
            children = List(),
            style = Style(border = true, fg = Blue)
          ),
          TextNode(2.x, 2.y, List(fit(s"Time: ${m.clock.value}").text)),
          TextNode(2.x, 3.y, List(fit(s"Random: ${m.random.value}").text)),
          TextNode(2.x, 4.y, List(("─" * innerWidth).text(fg = Red)))
        ) ++ messageNodes ++ List(
          TextNode(2.x, separatorY.y, List(("─" * innerWidth).text(fg = Blue))),
          TextNode(2.x, commandsStartY.y, List("Commands:".text(fg = Yellow))),
          TextNode(2.x, (commandsStartY + 1).y, List(fit("  start               -> start random numbers").text)),
          TextNode(2.x, (commandsStartY + 2).y, List(fit("  stop                -> stop random numbers").text)),
          TextNode(2.x, (commandsStartY + 3).y, List(fit("  clockstop | stopclock -> stop ticking").text)),
          TextNode(2.x, (commandsStartY + 4).y, List(fit("  exit                -> quit").text))
        ),
        input = Some(
          InputNode(
            2.x,
            inputY.y,
            renderedPrompt.text,
            Style(fg = Green),
            cursor = renderedPrompt.cursorIndex
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      input.value.trim.toLowerCase match
        case "start"                   => Right(StartRandom)
        case "stop"                    => Right(StopRandom)
        case "stopclock" | "clockstop" => Right(StopClock)
        case "exit"                    => Right(Exit)
        case other                     => Right(AddMessage(other))
