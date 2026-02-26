package termflow.tui

import java.util.concurrent.atomic.AtomicLong

private[tui] object RenderMetrics:
  private val enabled: Boolean =
    sys.env.get("TERMFLOW_RENDER_METRICS").exists(v => v.nonEmpty && v != "0" && !v.equalsIgnoreCase("false"))

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
      Console.err.println(
        s"[termflow-metrics] frames=${framesRendered.get()} changedCells=${changedCells.get()} " +
          s"bytes=${bytesEmitted.get()} coalescedFrames=${coalescedFrames.get()} coalescedCommands=${coalescedCmds.get()}"
      )
