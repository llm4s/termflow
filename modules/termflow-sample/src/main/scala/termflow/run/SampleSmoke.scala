package termflow.run

import termflow.tui._

import java.io.Reader
import java.io.StringReader

/** Non-interactive smoke checks for sample app wiring. */
object SampleSmoke:

  private object DummyTerminal extends TerminalBackend:
    override def reader: Reader = new StringReader("")
    override def width: Int     = 80
    override def height: Int    = 24
    override def close(): Unit  = ()

  private def dummyCtx[Msg]: RuntimeCtx[Msg] = new RuntimeCtx[Msg]:
    override def terminal: TerminalBackend            = DummyTerminal
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
    Console.out.println("Sample smoke checks passed.")
