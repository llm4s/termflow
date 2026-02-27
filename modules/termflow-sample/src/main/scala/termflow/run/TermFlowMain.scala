package termflow.run

import termflow.tui.TuiRuntime

/** Entry points for the sample apps. */
object TermFlowMain:

  def main(args: Array[String]): Unit =
    args.headOption.map(_.trim.toLowerCase) match
      case None | Some("") | Some("hub")          => SampleHubMain.main(Array.empty)
      case Some("echo")                           => TuiRuntime.run(termflow.apps.echo.EchoApp.App)
      case Some("sync-counter") | Some("sync")    => TuiRuntime.run(termflow.apps.counter.SyncCounter.App)
      case Some("future-counter") | Some("async") => TuiRuntime.run(termflow.apps.counter.FutureCounter.App)
      case Some("clock")                          => TuiRuntime.run(termflow.apps.clock.DigitalClock.App)
      case Some("clock-random")             => TuiRuntime.run(termflow.apps.clock.DigitalClockWithRandomSource.App)
      case Some("task")                     => TuiRuntime.run(termflow.apps.task.Task.App)
      case Some("stress")                   => TuiRuntime.run(termflow.apps.stress.RenderStressApp.App)
      case Some("sine") | Some("sine-wave") => TuiRuntime.run(termflow.apps.stress.SineWaveApp.App)
      case Some("tabs") | Some("tabs-demo") => TuiRuntime.run(termflow.apps.tabs.TabsDemoApp.App)
      case Some(_)                          => SampleHubMain.main(Array.empty)

  def syncCounterMain(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.counter.SyncCounter.App)

  def futureCounterMain(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.counter.FutureCounter.App)

  def taskMain(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.task.Task.App)

  def digitalClockMain(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.clock.DigitalClock.App)

  def digitalClockMainWithRandom(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.clock.DigitalClockWithRandomSource.App)

  def echoAppMain(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.echo.EchoApp.App)

  def renderStressMain(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.stress.RenderStressApp.App)

  def sineWaveMain(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.stress.SineWaveApp.App)

  def tabsDemoMain(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(termflow.apps.tabs.TabsDemoApp.App)

  def sampleHubMain(args: Array[String]): Unit =
    val _ = args
    SampleHubMain.main(Array.empty)
