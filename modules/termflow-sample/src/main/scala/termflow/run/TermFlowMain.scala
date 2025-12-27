package termflow.run

import termflow.tui.TuiRuntime

/** Entry points for the sample apps (Scala 2 style). */
object TermFlowMain {

  def syncCounterMain(args: Array[String]): Unit = {
    val _ = args
    TuiRuntime.run(termflow.apps.counter.SyncCounter.App)
  }

  def futureCounterMain(args: Array[String]): Unit = {
    val _ = args
    TuiRuntime.run(termflow.apps.counter.FutureCounter.App)
  }

  def taskMain(args: Array[String]): Unit = {
    val _ = args
    TuiRuntime.run(termflow.apps.task.Task.App)
  }

  def digitalClockMain(args: Array[String]): Unit = {
    val _ = args
    TuiRuntime.run(termflow.apps.clock.DigitalClock.App)
  }

  def digitalClockMainWithRandom(args: Array[String]): Unit = {
    val _ = args
    TuiRuntime.run(termflow.apps.clock.DigitalClockWithRandomSource.App)
  }

  def echoAppMain(args: Array[String]): Unit = {
    val _ = args
    TuiRuntime.run(termflow.apps.echo.EchoApp.App)
  }
}
