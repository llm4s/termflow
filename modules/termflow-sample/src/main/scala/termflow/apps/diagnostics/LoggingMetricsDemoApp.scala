package termflow.apps.diagnostics

import termflow.tui.*
import termflow.tui.Color.Cyan
import termflow.tui.Color.Green
import termflow.tui.Color.Yellow
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

import java.nio.file.Path

object LoggingMetricsDemoApp:
  private val MaxTicks = 8

  val DemoLogPath: LogPath =
    termflow.tui.LogPath(Path.of("target", "termflow-sample", "logging-metrics-demo.log"))

  val Config: TermFlowConfig =
    TermFlowConfig(
      logging = LoggingConfig(DemoLogPath),
      metrics = MetricsConfig(enabled = true)
    )

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    tick: Int,
    timer: Sub[Msg]
  )

  enum Msg:
    case Tick

  import Msg.*

  object App extends TuiApp[Model, Msg]:
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        tick = 0,
        timer = Sub.Every(75L, () => Tick, ctx)
      ).tui

    override def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Tick =>
          val nextTick = model.tick + 1
          val nextModel =
            model.copy(
              terminalWidth = ctx.terminal.width,
              terminalHeight = ctx.terminal.height,
              tick = nextTick
            )

          if nextTick >= MaxTicks then
            nextModel.timer.cancel()
            Tui(nextModel, Cmd.Exit)
          else nextModel.tui

    override def view(model: Model): RootNode =
      val boxWidth      = math.max(36, math.min(model.terminalWidth - 2, 72))
      val boxHeight     = 8
      val progressWidth = math.max(1, boxWidth - 6)
      val filled =
        math.max(0, math.min(progressWidth, (progressWidth.toDouble * model.tick / MaxTicks).toInt))
      val bar = ("#" * filled) + ("-" * (progressWidth - filled))

      RootNode(
        width = model.terminalWidth,
        height = math.max(model.terminalHeight, boxHeight + 2),
        children = List(
          BoxNode(1.x, 1.y, boxWidth, boxHeight, children = Nil, style = Style(border = true, fg = Cyan)),
          TextNode(3.x, 2.y, List("Logging + Metrics Demo".text(fg = Green, bg = Color.Default, bold = true))),
          TextNode(3.x, 4.y, List(s"Tick: ${model.tick}/$MaxTicks".text)),
          TextNode(3.x, 5.y, List(s"Progress: [$bar]".text(fg = Yellow))),
          TextNode(3.x, 7.y, List(s"Log file: ${DemoLogPath.path}".text))
        ),
        input = None
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation(s"Unexpected input: ${input.value}"))

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App, SimpleANSIRenderer(), new JLineTerminalBackend(), Config)
