package termflow.run

import termflow.apps.hub.SampleHubApp
import termflow.apps.hub.SampleHubApp.Choice
import termflow.tui.TuiRuntime

/** Interactive launcher for sample applications. */
object SampleHubMain:

  def main(args: Array[String]): Unit =
    val _       = args
    var running = true

    while running do
      var selected: Choice = Choice.Quit
      TuiRuntime.run(SampleHubApp.app(choice => selected = choice))

      selected match
        case Choice.Echo          => TuiRuntime.run(termflow.apps.echo.EchoApp.App)
        case Choice.SyncCounter   => TuiRuntime.run(termflow.apps.counter.SyncCounter.App)
        case Choice.FutureCounter => TuiRuntime.run(termflow.apps.counter.FutureCounter.App)
        case Choice.Clock         => TuiRuntime.run(termflow.apps.clock.DigitalClock.App)
        case Choice.ClockRandom   => TuiRuntime.run(termflow.apps.clock.DigitalClockWithRandomSource.App)
        case Choice.Task          => TuiRuntime.run(termflow.apps.task.Task.App)
        case Choice.Stress        => TuiRuntime.run(termflow.apps.stress.RenderStressApp.App)
        case Choice.Sine          => TuiRuntime.run(termflow.apps.stress.SineWaveApp.App)
        case Choice.Tabs          => TuiRuntime.run(termflow.apps.tabs.TabsDemoApp.App)
        case Choice.Quit          => running = false
