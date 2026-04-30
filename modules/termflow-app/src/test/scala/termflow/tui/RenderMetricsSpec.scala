package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files

class RenderMetricsSpec extends AnyFunSuite:

  private def newLog(prefix: String): (FrameworkLog, java.nio.file.Path) =
    val path = Files.createTempFile(prefix, ".log")
    (FrameworkLog(LoggingConfig(LogPath(path))), path)

  test("isEnabled reflects the supplied MetricsConfig"):
    val (log, _) = newLog("metrics-disabled")
    val off      = new RenderMetrics(MetricsConfig(enabled = false), log)
    assert(!off.isEnabled)
    val on = new RenderMetrics(MetricsConfig(enabled = true), log)
    assert(on.isEnabled)

  test("recordRender / recordCoalescing are no-ops when metrics are disabled"):
    val (log, path) = newLog("metrics-disabled-record")
    val m           = new RenderMetrics(MetricsConfig(enabled = false), log)
    m.recordRender(changed = 12, bytes = 200)
    m.recordCoalescing(commands = 4)
    m.printSummary() // disabled → emits nothing
    assert(Files.size(path) == 0L)

  test("recordRender accumulates and printSummary writes a metrics line"):
    val (log, path) = newLog("metrics-enabled")
    val m           = new RenderMetrics(MetricsConfig(enabled = true), log)
    m.recordRender(changed = 12, bytes = 200)
    m.recordRender(changed = 3, bytes = 50)
    m.recordCoalescing(commands = 2)
    m.recordCoalescing(commands = 0) // commands == 0 is dropped
    m.printSummary()
    val contents = Files.readString(path)
    assert(contents.contains("frames=2"), s"two frames recorded: $contents")
    assert(contents.contains("changedCells=15"), s"sum 12+3: $contents")
    assert(contents.contains("bytes=250"))
    assert(contents.contains("coalescedFrames=1"), s"one batch with commands>0: $contents")
    assert(contents.contains("coalescedCommands=2"))
