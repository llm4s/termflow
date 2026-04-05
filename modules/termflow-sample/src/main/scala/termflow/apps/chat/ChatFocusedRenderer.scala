package termflow.apps.chat

import termflow.tui.*

/**
 * A chat-focused renderer that avoids full-screen repaint on prompt cursor movement.
 *
 * This keeps resize handling conservative, but for normal typing it relies on
 * frame diffing rather than treating every cursor move as a structural repaint.
 */
final case class ChatFocusedRenderer() extends TuiRenderer:
  private var lastFrame: Option[AnsiRenderer.RenderFrame] = None

  override def render(
    textNode: RootNode,
    err: Option[TermFlowError],
    terminal: TerminalBackend,
    renderMetrics: RenderMetrics
  ): Unit =
    val _            = (err, renderMetrics)
    val currentFrame = AnsiRenderer.buildFrame(textNode)
    val resized      = lastFrame.exists(prev => prev.width != currentFrame.width || prev.height != currentFrame.height)
    val inputOnly =
      !resized && isInputOnlyChange(lastFrame, currentFrame, textNode)

    val fullAnsi =
      if inputOnly then AnsiRenderer.inputPatch(textNode)
      else
        val diffResult =
          if resized then AnsiRenderer.diff(None, currentFrame)
          else AnsiRenderer.diff(lastFrame, currentFrame)
        val ansi =
          if resized then ANSI.clearScreen + ANSI.homeCursor + diffResult.ansi
          else diffResult.ansi
        ansi

    if fullAnsi.nonEmpty then
      terminal.write(fullAnsi)
      terminal.flush()

    lastFrame = Some(currentFrame)

  private def isInputOnlyChange(
    previous: Option[AnsiRenderer.RenderFrame],
    current: AnsiRenderer.RenderFrame,
    root: RootNode
  ): Boolean =
    previous match
      case None => false
      case Some(prev) =>
        root.input match
          case None => false
          case Some(input) =>
            val inputRow = input.y.value - 1
            sameFrameOutsideInputRow(prev, current, inputRow)

  private def sameFrameOutsideInputRow(
    previous: AnsiRenderer.RenderFrame,
    current: AnsiRenderer.RenderFrame,
    inputRow: Int
  ): Boolean =
    if previous.width != current.width || previous.height != current.height then false
    else
      var row   = 0
      var equal = true
      while row < current.height && equal do
        if row != inputRow then
          var col = 0
          while col < current.width && equal do
            equal = previous.cells(row)(col) == current.cells(row)(col)
            col += 1
        row += 1
      equal
