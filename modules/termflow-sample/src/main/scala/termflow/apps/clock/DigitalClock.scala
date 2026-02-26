package termflow.apps.clock

import termflow.tui.Color.Blue
import termflow.tui.Color.Green
import termflow.tui.Color.Red
import termflow.tui.Color.Yellow
import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

import java.time.LocalTime

object DigitalClock:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

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
  enum Msg:
    case Tick
    case StartClock
    case StopClock
    case AddMessage(input: String)
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
        SubSource[String](
          Sub.Every(1000, () => Tick, ctx),
          LocalTime.now().toString
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

        case StartClock =>
          if sized.clock.sub.isActive then sized.copy(error = Some("Clock already running")).tui
          else sized.copy(clock = sized.clock.copy(sub = Sub.Every(1000, () => Tick, ctx)), error = None).tui

        case StopClock =>
          sized.clock.sub.cancel()
          sized.copy(clock = sized.clock.copy(sub = Sub.NoSub), messages = "Clock stopped" :: sized.messages).tui

        case AddMessage(input) =>
          sized.copy(messages = s"You said: $input" :: sized.messages).tui

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

      def fit(line: String): String =
        if line.length <= innerWidth then line
        else if innerWidth <= 3 then line.take(innerWidth)
        else line.take(innerWidth - 3) + "..."

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
          TextNode(2.x, 2.y, List(fit(s"Time: ${m.clock.value}").text)),
          TextNode(2.x, 3.y, List(("─" * innerWidth).text(fg = Red)))
        ) ++ m.messages.zipWithIndex.map { case (msg, idx) => TextNode(2.x, (4 + idx).y, List(fit(msg).text)) } ++ List(
          TextNode(2.x, (4 + m.messages.length).y, List(("─" * innerWidth).text(fg = Blue))),
          TextNode(2.x, (5 + m.messages.length).y, List(fit("Commands:").text(fg = Yellow))),
          TextNode(2.x, (6 + m.messages.length).y, List(fit("  stopclock  -> stop ticking").text)),
          TextNode(2.x, (7 + m.messages.length).y, List(fit("  startclock -> start ticking").text)),
          TextNode(2.x, (8 + m.messages.length).y, List(fit("  exit      -> quit").text))
        ),
        input = Some(
          InputNode(
            2.x,
            (9 + m.messages.length).y,
            renderedPrompt.text,
            Style(fg = Green),
            cursor = renderedPrompt.cursorIndex
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      input.value.trim.toLowerCase match
        case "startclock" => Right(StartClock)
        case "stopclock"  => Right(StopClock)
        case "exit"       => Right(Exit)
        case other        => Right(AddMessage(other))
