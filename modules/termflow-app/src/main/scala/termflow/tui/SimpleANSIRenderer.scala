package termflow.tui

/**
 * Stateful [[TuiRenderer]] that wraps [[AnsiRenderer]] for incremental
 * frame painting against a [[TerminalBackend]].
 *
 * Holds the previous [[AnsiRenderer.RenderFrame]] in private state and
 * decides per call whether to emit a diff or a full repaint, threading
 * the result through the supplied [[RenderMetrics]].
 *
 * When the runtime supplies a non-empty `err`, the renderer overlays a
 * red, bold banner across the top row of the frame *before* diffing, so
 * the banner participates in the normal diff/repaint pipeline. The next
 * frame without `err` paints over the banner with the app's own view.
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
    val baseFrame = AnsiRenderer.buildFrame(textNode)
    val currentFrame = err match
      case Some(e) => SimpleANSIRenderer.overlayErrorBanner(baseFrame, e)
      case None    => baseFrame
    val resized = lastFrame.exists(prev => prev.width != currentFrame.width || prev.height != currentFrame.height)
    val depth   = terminal.capabilities.colorDepth
    val ext     = terminal.capabilities.extendedStyles
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

object SimpleANSIRenderer:

  /**
   * Format a [[TermFlowError]] as the short, single-line text shown in the
   * runtime's error banner. Public so apps that ship a custom
   * [[TuiRenderer]] can match the default's wording.
   */
  def formatErrorBanner(err: TermFlowError): String = err match
    case TermFlowError.ConfigError(msg)    => s"Config error: $msg"
    case TermFlowError.ModelNotFound       => "Model not found"
    case TermFlowError.Unexpected(msg, _)  => s"Error: $msg"
    case TermFlowError.Validation(msg)     => s"Invalid input: $msg"
    case TermFlowError.CommandError(input) => s"Unrecognised command: $input"
    case TermFlowError.UnknownApp(name)    => s"Unknown app: $name"

  /**
   * Return a copy of `frame` with the top row replaced by a red, bold
   * banner that surfaces `err`. Wide-cell-safe: writes single-width cells
   * across the row and truncates long messages with an ellipsis. If the
   * frame has no rows or zero width the original frame is returned
   * unchanged.
   */
  def overlayErrorBanner(
    frame: AnsiRenderer.RenderFrame,
    err: TermFlowError
  ): AnsiRenderer.RenderFrame =
    if frame.height <= 0 || frame.width <= 0 then frame
    else
      val text = formatErrorBanner(err)
      // Reserve one leading space; truncate with an ellipsis when the
      // message is wider than the remaining banner width.
      val budget = math.max(0, frame.width - 1)
      val body =
        if text.length <= budget then text
        else if budget <= 1 then text.take(budget)
        else text.take(budget - 1) + "…"
      val padded = (" " + body).padTo(frame.width, ' ').take(frame.width)
      val style  = Style(fg = Color.White, bg = Color.Red, bold = true)
      val newRow = Array.tabulate(frame.width) { col =>
        val ch = if col < padded.length then padded.charAt(col) else ' '
        AnsiRenderer.RenderCell(ch, style, 1)
      }
      val newCells = frame.cells.clone()
      newCells(0) = newRow
      frame.copy(cells = newCells)
