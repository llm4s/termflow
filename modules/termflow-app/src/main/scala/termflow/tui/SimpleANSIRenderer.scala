package termflow.tui

/**
 * Stateful [[TuiRenderer]] that wraps [[AnsiRenderer]] for incremental
 * frame painting against a [[TerminalBackend]].
 *
 * Holds the previous [[AnsiRenderer.RenderFrame]] in private state and
 * decides per call whether to emit a diff or a full repaint, threading
 * the result through the supplied [[RenderMetrics]].
 *
 * Lives in `termflow-app` because it depends on the app-layer types
 * `TuiRenderer`, `TermFlowError`, and `RenderMetrics`. The pure
 * `AnsiRenderer.{buildFrame, diff}` primitives stay in
 * `termflow-screen`, where they remain trivially unit-testable
 * without a backend.
 */
final case class SimpleANSIRenderer() extends TuiRenderer:
  private var lastFrame: Option[AnsiRenderer.RenderFrame] = None
  private val FullRepaintRowThreshold                     = 6

  override def render(
    textNode: RootNode,
    err: Option[TermFlowError],
    terminal: TerminalBackend,
    renderMetrics: RenderMetrics
  ): Unit =
    val currentFrame = AnsiRenderer.buildFrame(textNode)
    val resized      = lastFrame.exists(prev => prev.width != currentFrame.width || prev.height != currentFrame.height)
    val depth        = terminal.capabilities.colorDepth
    val ext          = terminal.capabilities.extendedStyles
    val initialDiff =
      if resized then AnsiRenderer.diff(None, currentFrame, depth, ext)
      else AnsiRenderer.diff(lastFrame, currentFrame, depth, ext)
    val shouldFullRepaint =
      resized || initialDiff.changedRows >= math.min(currentFrame.height, FullRepaintRowThreshold)
    val diffResult =
      if shouldFullRepaint then AnsiRenderer.diff(None, currentFrame, depth, ext)
      else initialDiff
    val ansi =
      if shouldFullRepaint then ANSI.clearScreen + ANSI.homeCursor + diffResult.ansi
      else diffResult.ansi
    if ansi.nonEmpty then
      terminal.write(ansi)
      terminal.flush()
    if renderMetrics.isEnabled then
      val bytes = ansi.getBytes("UTF-8").length
      renderMetrics.recordRender(diffResult.changedCells, bytes)
    lastFrame = Some(currentFrame)
    val _ = err
