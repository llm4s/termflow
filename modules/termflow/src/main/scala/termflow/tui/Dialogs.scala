package termflow.tui

/**
 * Builders for common modal dialogs as [[Overlay]] values.
 *
 * Each helper returns an `Overlay` ready to drop into
 * [[RootNode.overlays]]. The dialog state (open / closed, selected button,
 * entered text) lives in the app's own model — these helpers are pure
 * presentation, mirroring the rest of TermFlow's view layer.
 *
 * The shipped helpers all use `InputCapture.Modal` so the base view's
 * cursor and input are suppressed while the dialog is up. Apps decide
 * whether to also gate their own `update` on the dialog being open
 * (typically by pattern-matching on `model.dialog`).
 */
object Dialogs:

  /**
   * Choice button rendered in the action row of a dialog.
   *
   * @param label   Visible label (e.g. `"OK"`, `"Cancel"`).
   * @param focused Whether this button currently owns focus.
   */
  final case class Choice(label: String, focused: Boolean)

  /**
   * Build a centred message dialog with one or more action buttons.
   *
   * Layout: bordered box, title row at the top, body text in the middle,
   * a horizontal action row at the bottom with the supplied [[Choice]]s
   * separated by two cells.
   *
   * Sizing: width is computed from the longest of (title + padding),
   * (body + padding), (sum of choice widths + gaps), with a minimum of
   * 40 cells. Height is 6 (top border + title + blank + body + actions +
   * bottom border).
   *
   * Apps drive button focus / activation via their own keymap; this
   * helper only renders the visual.
   *
   * @param title    Title rendered on the top border, in the theme's primary slot.
   * @param body     One or more body lines, in the theme's foreground slot.
   * @param choices  Action buttons, drawn left-to-right.
   * @param position Anchor (default [[OverlayPosition.Centered]]).
   * @param theme    Ambient theme (chars + colours).
   */
  def message(
    title: String,
    body: List[String],
    choices: List[Choice],
    position: OverlayPosition = OverlayPosition.Centered
  )(using theme: Theme): Overlay =
    val titleLen   = title.length + 4 // " <title> "
    val bodyLen    = (0 :: body.map(_.length)).max + 4
    val actionsLen = choiceWidth(choices) + 4
    val width      = math.max(40, math.max(titleLen, math.max(bodyLen, actionsLen)))
    val height     = math.max(7, 4 + body.size)

    // Local coords inside the overlay: (1.x, 1.y) is the overlay's top-left.
    val box       = Theme.box(XCoord(1), YCoord(1), width, height)
    val titleNode = TextNode(XCoord(3), YCoord(1), List(Text(s" $title ", Style(fg = theme.primary, bold = true))))
    val bodyNodes: List[VNode] =
      body.zipWithIndex.map { case (line, i) =>
        TextNode(XCoord(3), YCoord(3 + i), List(Text(line, Style(fg = theme.foreground))))
      }
    val actionsRow = renderActions(choices, width, height - 1, theme)

    Overlay(
      position = position,
      width = width,
      height = height,
      children = box :: titleNode :: bodyNodes ++ List(actionsRow),
      inputCapture = InputCapture.Modal
    )

  /**
   * Convenience wrapper around [[message]] for the common "yes/no" case.
   *
   * @param prompt  Single-line question rendered as the body.
   * @param yesFocused Which button starts focused. `true` highlights "Yes",
   *                   `false` highlights "No". Default `false` (the safer
   *                   destructive-action convention).
   */
  def confirm(
    prompt: String,
    yesFocused: Boolean = false,
    title: String = "Confirm",
    yesLabel: String = "Yes",
    noLabel: String = "No",
    position: OverlayPosition = OverlayPosition.Centered
  )(using Theme): Overlay =
    message(
      title = title,
      body = List(prompt),
      choices = List(Choice(yesLabel, yesFocused), Choice(noLabel, !yesFocused)),
      position = position
    )

  // ---- internals -----------------------------------------------------------

  private def choiceWidth(choices: List[Choice]): Int =
    if choices.isEmpty then 0
    else choices.map(c => c.label.length + 4).sum + (choices.size - 1) * 2

  private def renderActions(choices: List[Choice], boxWidth: Int, row: Int, theme: Theme): VNode =
    val totalLen = choiceWidth(choices)
    val startX   = math.max(2, ((boxWidth - totalLen) / 2) + 1)
    val segments: List[Text] =
      choices.zipWithIndex.flatMap { case (c, idx) =>
        val fg    = if c.focused then theme.primary else theme.foreground
        val bold  = c.focused
        val label = if c.focused then s"[ ${c.label} ]" else s"  ${c.label}  "
        val gap   = if idx == choices.size - 1 then "" else "  "
        List(Text(label, Style(fg = fg, bold = bold)), Text(gap, Style()))
      }
    TextNode(XCoord(startX), YCoord(row), segments)
