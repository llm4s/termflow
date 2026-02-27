package termflow.apps.hub

import termflow.tui.Color.Blue
import termflow.tui.Color.Green
import termflow.tui.Color.Yellow
import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

object SampleHubApp:

  enum Choice:
    case Echo
    case SyncCounter
    case FutureCounter
    case Clock
    case ClockRandom
    case Task
    case Stress
    case Sine
    case Tabs
    case Quit

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    message: String,
    input: Sub[Msg],
    prompt: Prompt.State
  )

  enum Msg:
    case Launch(choice: Choice)
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)

  import Choice._
  import Msg._

  def app(onLaunch: Choice => Unit): TuiApp[Model, Msg] =
    new TuiApp[Model, Msg]:
      private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
        val w = ctx.terminal.width
        val h = ctx.terminal.height
        if w == m.terminalWidth && h == m.terminalHeight then m
        else m.copy(terminalWidth = w, terminalHeight = h)

      override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
        Model(
          terminalWidth = ctx.terminal.width,
          terminalHeight = ctx.terminal.height,
          message = "Choose an app by name or number (for example: 1, echo, tabs, exit).",
          input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
          prompt = Prompt.State()
        ).tui

      override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
        val sized = syncTerminalSize(m, ctx)
        msg match
          case Launch(choice) =>
            onLaunch(choice)
            Tui(sized, Cmd.Exit)

          case ConsoleInputKey(k) =>
            val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](sized.prompt, k)(toMsg)
            maybeCmd match
              case Some(cmd) => Tui(sized.copy(prompt = nextPrompt), cmd)
              case None      => sized.copy(prompt = nextPrompt).tui

          case ConsoleInputError(e) =>
            sized.copy(message = s"input error: ${Option(e.getMessage).getOrElse("unknown")}").tui

      override def view(m: Model): RootNode =
        val prefix         = "[]> "
        val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
        val boxWidth       = math.max(2, m.terminalWidth - 4)
        val innerWidth     = math.max(1, boxWidth - 2)
        val menu = List(
          "1  | echo           -> Echo app",
          "2  | sync           -> SyncCounter",
          "3  | async          -> FutureCounter",
          "4  | clock          -> DigitalClock",
          "5  | clock-random   -> DigitalClockWithRandomSource",
          "6  | task           -> Task manager",
          "7  | stress         -> RenderStressApp",
          "8  | sine           -> SineWaveApp",
          "9  | tabs           -> Tabs demo",
          "0  | exit           -> quit hub"
        ).map(line => if line.length <= innerWidth then line else line.take(innerWidth))

        val status    = if m.message.length <= innerWidth then m.message else m.message.take(innerWidth)
        val boxHeight = 5 + menu.length + 4
        val children: List[VNode] = List(
          BoxNode(1.x, 1.y, boxWidth, boxHeight, children = Nil, style = Style(border = true, fg = Blue)),
          TextNode(2.x, 2.y, List(Text("Sample Hub", Style(fg = Yellow, bold = true, underline = true)))),
          TextNode(2.x, 3.y, List(Text(status, Style(fg = Green)))),
          TextNode(2.x, 4.y, List(Text("Commands:", Style(fg = Yellow))))
        ) ++ menu.zipWithIndex.map { case (line, idx) =>
          TextNode(2.x, (5 + idx).y, List(line.text))
        }

        RootNode(
          width = m.terminalWidth,
          height = boxHeight + 4,
          children = children,
          input = Some(
            InputNode(
              2.x,
              (boxHeight + 2).y,
              renderedPrompt.text,
              Style(fg = Green),
              cursor = renderedPrompt.cursorIndex
            )
          )
        )

      override def toMsg(input: PromptLine): Result[Msg] =
        input.value.trim.toLowerCase match
          case "1" | "echo"          => Right(Launch(Echo))
          case "2" | "sync"          => Right(Launch(SyncCounter))
          case "3" | "async"         => Right(Launch(FutureCounter))
          case "4" | "clock"         => Right(Launch(Clock))
          case "5" | "clock-random"  => Right(Launch(ClockRandom))
          case "6" | "task"          => Right(Launch(Task))
          case "7" | "stress"        => Right(Launch(Stress))
          case "8" | "sine"          => Right(Launch(Sine))
          case "9" | "tabs"          => Right(Launch(Tabs))
          case "0" | "exit" | "quit" => Right(Launch(Quit))
          case other                 => Left(TermFlowError.Validation(s"Unknown app: $other"))
