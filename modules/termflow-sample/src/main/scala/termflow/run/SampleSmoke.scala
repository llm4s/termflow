package termflow.run

import termflow.tui.*

import java.io.Reader
import java.io.StringReader
import java.io.StringWriter
import java.nio.file.Path

/** Non-interactive smoke checks for sample app wiring. */
object SampleSmoke:
  private val DummyConfig =
    TermFlowConfig(
      logging = LoggingConfig(LogPath(Path.of("target", "termflow-smoke.log"))),
      metrics = MetricsConfig(enabled = false)
    )

  private object DummyTerminal extends TerminalBackend:
    override def reader: Reader = new StringReader("")
    override def writer         = new StringWriter()
    override def width: Int     = 80
    override def height: Int    = 24
    override def close(): Unit  = ()

  private def dummyCtx[Msg]: RuntimeCtx[Msg] = new RuntimeCtx[Msg]:
    override def terminal: TerminalBackend            = DummyTerminal
    override def config: TermFlowConfig               = DummyConfig
    override def publish(cmd: Cmd[Msg]): Unit         = ()
    override def registerSub(sub: Sub[Msg]): Sub[Msg] = sub

  private def smokeApp[Model, Msg](name: String, app: TuiApp[Model, Msg]): Unit =
    val initial = app.init(dummyCtx[Msg])
    val view    = app.view(initial.model)
    if view.width <= 0 || view.height <= 0 then
      throw new IllegalStateException(s"Invalid root dimensions for sample '$name': ${view.width}x${view.height}")

  def main(args: Array[String]): Unit =
    val _ = args
    smokeApp("echo", termflow.apps.echo.EchoApp.App)
    smokeApp("future-counter", termflow.apps.counter.FutureCounter.App)
    smokeApp("clock", termflow.apps.clock.DigitalClock.App)
    smokeApp("stress", termflow.apps.stress.RenderStressApp.App)
    smokeApp("tabs", termflow.apps.tabs.TabsDemoApp.App)
    smokeApp("input-line", termflow.apps.input.InputLineReproApp.App)
    smokeApp("widgets-demo", termflow.apps.widgets.WidgetsDemoApp.App)
    smokeApp("form-demo", termflow.apps.forms.FormDemoApp.App)
    smokeApp("catalog-demo", termflow.apps.catalog.CatalogDemoApp.App)
    Console.out.println("Sample smoke checks passed.")
