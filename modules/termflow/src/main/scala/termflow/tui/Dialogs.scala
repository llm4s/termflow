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

  /**
   * Centred text-input dialog. Renders the prompt label above an
   * [[InputNode]] and an OK / Cancel action row. The dialog owns the
   * cursor while open (`InputCapture.Modal`); the app drives editing by
   * feeding `KeyEvent`s into [[Prompt.handleKey]] over its own
   * `Prompt.State` and rebuilding the dialog each frame.
   *
   * State (current text, cursor position, focused button) lives in the
   * app's model, exactly like the rest of the dialog helpers. This keeps
   * the helper purely presentational and testable in isolation.
   *
   * @param title       Title rendered on the top border.
   * @param prompt      Label drawn above the input (e.g. "Username:").
   * @param value       Current contents of the input field.
   * @param cursor      Cursor index into `value` (`-1` means "at end").
   * @param okFocused   Which action button starts focused. `true` highlights OK.
   * @param okLabel     Label for the confirm button. Defaults to "OK".
   * @param cancelLabel Label for the cancel button. Defaults to "Cancel".
   * @param prefix      Optional fixed prefix (e.g. "> ") pinned to the
   *                    left edge of the input viewport even while
   *                    horizontally scrolling. Empty means none.
   * @param position    Anchor (default [[OverlayPosition.Centered]]).
   */
  def textInput(
    title: String,
    prompt: String,
    value: String,
    cursor: Int = -1,
    okFocused: Boolean = true,
    okLabel: String = "OK",
    cancelLabel: String = "Cancel",
    prefix: String = "",
    position: OverlayPosition = OverlayPosition.Centered
  )(using theme: Theme): Overlay =
    val choices    = List(Choice(okLabel, okFocused), Choice(cancelLabel, !okFocused))
    val titleLen   = title.length + 4
    val promptLen  = prompt.length + 4
    val actionsLen = choiceWidth(choices) + 4
    val minInner   = math.max(prefix.length + value.length + 4, 24)
    val width      = math.max(40, math.max(titleLen, math.max(promptLen, math.max(actionsLen, minInner + 4))))
    val height     = 8 // top + title + blank + label + input + blank + actions + bottom

    val box        = Theme.box(XCoord(1), YCoord(1), width, height)
    val titleNode  = TextNode(XCoord(3), YCoord(1), List(Text(s" $title ", Style(fg = theme.primary, bold = true))))
    val promptNode = TextNode(XCoord(3), YCoord(3), List(Text(prompt, Style(fg = theme.foreground))))
    val inputNode: InputNode = VNode.InputNode(
      x = XCoord(3),
      y = YCoord(4),
      prompt = prefix + value,
      style = Style(fg = theme.foreground, bg = theme.background),
      cursor = if cursor < 0 then prefix.length + value.length else prefix.length + cursor,
      lineWidth = math.max(1, width - 4),
      prefixLength = prefix.length
    )
    val actionsRow = renderActions(choices, width, height - 1, theme)

    Overlay(
      position = position,
      width = width,
      height = height,
      children = List(box, titleNode, promptNode, actionsRow),
      input = Some(inputNode),
      inputCapture = InputCapture.Modal
    )

  /**
   * Centred list-select dialog. Renders a scrollable list of items with
   * the selected row highlighted in the theme's primary palette plus an
   * OK / Cancel action row.
   *
   * Apps drive arrow-key navigation by updating `selectedIndex` in their
   * own model and re-rendering. The helper handles viewport scrolling so
   * the selection always stays visible — selecting an item below the
   * fold scrolls the list down.
   *
   * @param title         Title rendered on the top border.
   * @param items         Items to render. Each is shown via its `toString`
   *                      unless `render` is supplied.
   * @param selectedIndex Index of the highlighted row.
   * @param okFocused     Action-button focus.
   * @param maxVisible    Number of list rows visible at once. Defaults to 8.
   * @param render        Display function for an item.
   * @param okLabel       Label for the confirm button.
   * @param cancelLabel   Label for the cancel button.
   * @param position      Anchor (default [[OverlayPosition.Centered]]).
   */
  def listSelect[A](
    title: String,
    items: Seq[A],
    selectedIndex: Int,
    okFocused: Boolean = true,
    maxVisible: Int = 8,
    render: A => String = (a: A) => a.toString,
    okLabel: String = "OK",
    cancelLabel: String = "Cancel",
    position: OverlayPosition = OverlayPosition.Centered
  )(using theme: Theme): Overlay =
    val visible    = math.max(1, math.min(maxVisible, math.max(1, items.size)))
    val choices    = List(Choice(okLabel, okFocused), Choice(cancelLabel, !okFocused))
    val titleLen   = title.length + 4
    val actionsLen = choiceWidth(choices) + 4
    val itemLen    = (0 +: items.map(a => render(a).length)).max + 6
    val width      = math.max(40, math.max(titleLen, math.max(actionsLen, itemLen)))
    val height     = 5 + visible

    // Viewport: scroll so the selected row stays visible. We anchor the
    // first visible row so that selection is centred when possible, but
    // never past the end of the list.
    val anchor =
      if items.isEmpty then 0
      else
        val sel    = math.max(0, math.min(items.size - 1, selectedIndex))
        val maxTop = math.max(0, items.size - visible)
        val raw    = sel - visible / 2
        math.max(0, math.min(maxTop, raw))

    val box = Theme.box(XCoord(1), YCoord(1), width, height)
    val titleNode =
      TextNode(XCoord(3), YCoord(1), List(Text(s" $title ", Style(fg = theme.primary, bold = true))))

    val rows: List[VNode] =
      items
        .slice(anchor, anchor + visible)
        .zipWithIndex
        .map { case (item, i) =>
          val absIdx = anchor + i
          val sel    = absIdx == selectedIndex
          val marker = if sel then "▸ " else "  "
          val style =
            if sel then Style(fg = theme.background, bg = theme.primary, bold = true)
            else Style(fg = theme.foreground)
          TextNode(XCoord(3), YCoord(3 + i), List(Text(s"$marker${render(item)}", style)))
        }
        .toList

    val actionsRow = renderActions(choices, width, height - 1, theme)

    Overlay(
      position = position,
      width = width,
      height = height,
      children = (box :: titleNode :: rows) :+ actionsRow,
      inputCapture = InputCapture.Modal
    )

  /**
   * Animated frames for [[waiting]]. Apps that don't pass a custom set
   * get this Braille spinner — the same one used by widgets `Spinner`.
   */
  val defaultSpinnerFrames: Vector[String] =
    Vector("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")

  /**
   * Compact "busy" overlay for long-running async work. Centred, with a
   * spinner glyph chosen from `frames` by `tick` (modulo). Apps drive the
   * tick from a [[Sub.Every]] and pop the dialog when the underlying
   * future completes.
   *
   * Modal by default — the user shouldn't be able to interact with the
   * base view while a blocking task is in flight. Pass a `cancelLabel`
   * if the operation is cancellable; the app handles the key/click and
   * removes the overlay.
   *
   * @param title       Title rendered on the top border.
   * @param body        Single-line status message.
   * @param tick        Animation tick — typically incremented by a `Sub.Every`.
   * @param frames      Spinner frames to cycle through.
   * @param cancelLabel If set, renders a single focused action button so
   *                    the user can interrupt. Defaults to none.
   * @param position    Anchor (default [[OverlayPosition.Centered]]).
   */
  def waiting(
    title: String,
    body: String,
    tick: Long,
    frames: Vector[String] = defaultSpinnerFrames,
    cancelLabel: Option[String] = None,
    position: OverlayPosition = OverlayPosition.Centered
  )(using theme: Theme): Overlay =
    val activeFrames = if frames.isEmpty then defaultSpinnerFrames else frames
    val frame        = activeFrames(((tick % activeFrames.length).toInt + activeFrames.length) % activeFrames.length)
    val choices      = cancelLabel.toList.map(label => Choice(label, focused = true))
    val titleLen     = title.length + 4
    val bodyLen      = body.length + 4 + 4 // " spinner  body  "
    val actionsLen   = if choices.isEmpty then 0 else choiceWidth(choices) + 4
    val width        = math.max(40, math.max(titleLen, math.max(bodyLen, actionsLen)))
    val height       = if choices.isEmpty then 5 else 7

    val box = Theme.box(XCoord(1), YCoord(1), width, height)
    val titleNode =
      TextNode(XCoord(3), YCoord(1), List(Text(s" $title ", Style(fg = theme.primary, bold = true))))
    val statusNode =
      TextNode(
        XCoord(3),
        YCoord(3),
        List(
          Text(s"$frame ", Style(fg = theme.primary, bold = true)),
          Text(body, Style(fg = theme.foreground))
        )
      )

    val children =
      if choices.isEmpty then List(box, titleNode, statusNode)
      else List(box, titleNode, statusNode, renderActions(choices, width, height - 1, theme))

    Overlay(
      position = position,
      width = width,
      height = height,
      children = children,
      inputCapture = InputCapture.Modal
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
