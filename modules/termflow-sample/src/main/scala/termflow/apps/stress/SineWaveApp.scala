package termflow.apps.stress

import termflow.tui.Color
import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

import scala.math.Pi
import scala.math.sin

object SineWaveApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    phase: Double,
    step: Double,
    running: Boolean,
    ticker: Sub[Msg],
    input: Sub[Msg],
    prompt: Prompt.State,
    status: String
  )

  enum Msg:
    case Tick
    case Pause
    case Resume
    case Faster
    case Slower
    case Exit
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        phase = 0.0,
        step = 0.22,
        running = true,
        // 20 FPS is visually smooth but reduces cursor/prompt jitter under heavy updates.
        ticker = Sub.Every(50, () => Tick, ctx),
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State(),
        status = "running"
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Tick =>
          if m.running then m.copy(phase = m.phase + m.step).tui else m.tui

        case Pause =>
          if m.ticker.isActive then m.ticker.cancel()
          m.copy(running = false, ticker = Sub.NoSub, status = "paused").tui

        case Resume =>
          if m.running then m.copy(status = "already running").tui
          else m.copy(running = true, ticker = Sub.Every(50, () => Tick, ctx), status = "running").tui

        case Faster =>
          val next = math.min(0.8, m.step + 0.05)
          m.copy(step = next, status = f"speed=${next}%.2f").tui

        case Slower =>
          val next = math.max(0.05, m.step - 0.05)
          m.copy(step = next, status = f"speed=${next}%.2f").tui

        case Exit =>
          if m.ticker.isActive then m.ticker.cancel()
          Tui(m, Cmd.Exit)

        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(m.copy(prompt = nextPrompt), cmd)
            case None      => m.copy(prompt = nextPrompt).tui

        case ConsoleInputError(e) =>
          m.copy(status = s"input error: ${Option(e.getMessage).getOrElse("unknown")}").tui

    override def view(m: Model): RootNode =
      val width          = math.max(40, m.terminalWidth)
      val height         = math.max(16, m.terminalHeight)
      val boxHeight      = math.max(10, height - 5)
      val boxWidth       = math.max(2, width - 4)
      val innerWidth     = math.max(1, boxWidth - 2)
      val innerHeight    = math.max(3, boxHeight - 2)
      val baseline       = innerHeight / 2
      val amplitude      = math.max(1, innerHeight / 3)
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, "[]> ")

      def clamp(v: Int, lo: Int, hi: Int): Int = math.max(lo, math.min(hi, v))

      val rows = Array.fill(innerHeight)(Array.fill(innerWidth)(' '))

      var x = 0
      while x < innerWidth do
        val t  = m.phase + (x.toDouble / innerWidth.toDouble) * 4.0 * Pi
        val y0 = baseline + math.round(sin(t) * amplitude).toInt
        val y  = clamp(y0, 0, innerHeight - 1)
        rows(y)(x) = '*'

        // add a second, dimmer harmonic trail for visual depth
        val t2  = t * 1.8
        val y20 = baseline + math.round(sin(t2) * (amplitude * 0.5)).toInt
        val y2  = clamp(y20, 0, innerHeight - 1)
        if rows(y2)(x) == ' ' then rows(y2)(x) = '.'
        x += 1

      val waveNodes =
        rows.zipWithIndex.toList.map { case (arr, idx) =>
          val line  = new String(arr)
          val style = if idx % 2 == 0 then Style(fg = Color.Cyan) else Style(fg = Color.Blue)
          TextNode(2.x, (2 + idx).y, List(Text(line, style)))
        }

      val statusLine   = f"status=${m.status}  step=${m.step}%.2f"
      val fittedStatus = if statusLine.length <= innerWidth then statusLine else statusLine.take(innerWidth)

      RootNode(
        width = width,
        height = height,
        children = List(
          BoxNode(1.x, 1.y, boxWidth, boxHeight, children = Nil, style = Style(border = true, fg = Color.Magenta)),
          TextNode(2.x, 2.y, List(Text(fittedStatus, Style(fg = Color.Yellow, bold = true))))
        ) ++ waveNodes ++ List(
          TextNode(
            2.x,
            (boxHeight + 1).y,
            List(Text("Commands: pause | resume | faster | slower | exit", Style(fg = Color.White)))
          )
        ),
        input = Some(
          InputNode(
            2.x,
            (boxHeight + 3).y,
            renderedPrompt.text,
            Style(fg = Color.Green),
            cursor = renderedPrompt.cursorIndex,
            lineWidth = innerWidth
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      input.value.trim.toLowerCase match
        case "pause"  => Right(Pause)
        case "resume" => Right(Resume)
        case "faster" => Right(Faster)
        case "slower" => Right(Slower)
        case "exit"   => Right(Exit)
        case other    => Left(TermFlowError.Validation(s"Unknown command: $other"))
