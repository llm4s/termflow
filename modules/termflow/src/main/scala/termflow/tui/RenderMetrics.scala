package termflow.tui

import java.util.concurrent.atomic.AtomicLong

final class RenderMetrics private[tui] (config: MetricsConfig, logger: FrameworkLog):
  private val enabled         = config.enabled
  private val framesRendered  = new AtomicLong(0L)
  private val changedCells    = new AtomicLong(0L)
  private val bytesEmitted    = new AtomicLong(0L)
  private val coalescedFrames = new AtomicLong(0L)
  private val coalescedCmds   = new AtomicLong(0L)

  def isEnabled: Boolean = enabled

  def recordRender(changed: Int, bytes: Int): Unit =
    if enabled then
      framesRendered.incrementAndGet()
      changedCells.addAndGet(changed.toLong)
      bytesEmitted.addAndGet(bytes.toLong): Unit

  def recordCoalescing(commands: Int): Unit =
    if enabled && commands > 0 then
      coalescedFrames.incrementAndGet()
      coalescedCmds.addAndGet(commands.toLong): Unit

  def printSummary(): Unit =
    if enabled then
      val _ = logger.info(
        s"[termflow-metrics] frames=${framesRendered.get()} changedCells=${changedCells.get()} " +
          s"bytes=${bytesEmitted.get()} coalescedFrames=${coalescedFrames.get()} coalescedCommands=${coalescedCmds.get()}"
      )
