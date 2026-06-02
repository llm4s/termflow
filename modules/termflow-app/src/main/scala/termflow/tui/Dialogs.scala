package termflow.tui

import termflow.tui.TuiPrelude.Result

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Try

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
  /**
   * Layout descriptor for a `listSelect` dialog. Apps wiring mouse
   * hit-testing should use this to map a click row to an item index
   * without re-deriving (and drifting from) the helper's anchor math.
   *
   * @param visibleCount Number of list rows actually drawn.
   * @param anchorIndex  Index of the first visible item — selection
   *                     scrolls relative to this.
   * @param firstRowOffset Y-offset of the first list row inside the
   *                       overlay rectangle (panel-local). The first row
   *                       is at overlay-absolute Y = `overlay.row +
   *                       firstRowOffset - 1` once positioned.
   */
  final case class ListSelectLayout(
    visibleCount: Int,
    anchorIndex: Int,
    firstRowOffset: Int
  )

  /** First selectable row inside a listSelect dialog (panel-local Y). */
  private val ListSelectFirstRowOffset: Int = 3

  /**
   * Compute the visible-window math `listSelect` uses internally so apps
   * can hit-test mouse clicks without copying the formula.
   *
   * @param itemsSize     Number of items the dialog will be built over.
   * @param selectedIndex Currently-selected item.
   * @param maxVisible    Same value passed to [[listSelect]].
   */
  def listSelectLayout(itemsSize: Int, selectedIndex: Int, maxVisible: Int = 8): ListSelectLayout =
    val visible = math.max(1, math.min(maxVisible, math.max(1, itemsSize)))
    val anchor =
      if itemsSize == 0 then 0
      else
        val sel    = math.max(0, math.min(itemsSize - 1, selectedIndex))
        val maxTop = math.max(0, itemsSize - visible)
        val raw    = sel - visible / 2
        math.max(0, math.min(maxTop, raw))
    ListSelectLayout(visible, anchor, ListSelectFirstRowOffset)

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
    val layout  = listSelectLayout(items.size, selectedIndex, maxVisible)
    val visible = layout.visibleCount
    val anchor  = layout.anchorIndex
    // Clamp to match the scroll anchor (which already clamps). Without this
    // an out-of-range `selectedIndex` (e.g. the app's list shrank before it
    // re-clamped) scrolls to a valid window but highlights no row, so the
    // list looks frozen with nothing selected.
    val clampedSelectedIndex =
      if items.isEmpty then -1 else math.max(0, math.min(items.size - 1, selectedIndex))
    val choices    = List(Choice(okLabel, okFocused), Choice(cancelLabel, !okFocused))
    val titleLen   = title.length + 4
    val actionsLen = choiceWidth(choices) + 4
    val itemLen    = (0 +: items.map(a => render(a).length)).max + 6
    val width      = math.max(40, math.max(titleLen, math.max(actionsLen, itemLen)))
    val height     = 5 + visible

    val box = Theme.box(XCoord(1), YCoord(1), width, height)
    val titleNode =
      TextNode(XCoord(3), YCoord(1), List(Text(s" $title ", Style(fg = theme.primary, bold = true))))

    val rows: List[VNode] =
      items
        .slice(anchor, anchor + visible)
        .zipWithIndex
        .map { case (item, i) =>
          val absIdx = anchor + i
          val sel    = absIdx == clampedSelectedIndex
          val marker = if sel then "▸ " else "  "
          val style =
            if sel then Style(fg = theme.background, bg = theme.primary, bold = true)
            else Style(fg = theme.foreground)
          TextNode(XCoord(3), YCoord(layout.firstRowOffset + i), List(Text(s"$marker${render(item)}", style)))
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
   * Centred action-list dialog. A vertical menu of `Choice` actions; the
   * one with `focused = true` is highlighted in the theme's primary
   * palette. Apps drive arrow-key navigation by toggling which `Choice`
   * is focused in their own model and re-rendering.
   *
   * This is a thin convenience over [[listSelect]] tailored for "pick an
   * action" menus: the items are `Choice` values (so they already carry
   * a label and a focused flag), there is no OK / Cancel row, and the
   * dialog auto-sizes to its longest label. Use [[listSelect]] when
   * you're choosing a value (with explicit OK / Cancel) rather than
   * triggering an action.
   *
   * Sizing: width is the max of (title + padding) and (longest label +
   * padding) with a 30-cell floor. Height is `4 + actions.size`.
   *
   * @param title    Title rendered on the top border.
   * @param actions  Action items. Exactly one should have
   *                 `focused = true`; if none are focused, the first row
   *                 still renders as the selection-target style.
   * @param position Anchor (default [[OverlayPosition.Centered]]).
   */
  def actionList(
    title: String,
    actions: List[Choice],
    position: OverlayPosition = OverlayPosition.Centered
  )(using theme: Theme): Overlay =
    val titleLen = title.length + 4
    val itemLen  = (0 :: actions.map(_.label.length)).max + 6
    val width    = math.max(30, math.max(titleLen, itemLen))
    val height   = 4 + math.max(1, actions.size)

    val box       = Theme.box(XCoord(1), YCoord(1), width, height)
    val titleNode = TextNode(XCoord(3), YCoord(1), List(Text(s" $title ", Style(fg = theme.primary, bold = true))))

    val rows: List[VNode] =
      actions.zipWithIndex.map { case (action, i) =>
        val sel    = action.focused
        val marker = if sel then "▸ " else "  "
        val style =
          if sel then Style(fg = theme.background, bg = theme.primary, bold = true)
          else Style(fg = theme.foreground)
        TextNode(XCoord(3), YCoord(3 + i), List(Text(s"$marker${action.label}", style)))
      }

    Overlay(
      position = position,
      width = width,
      height = height,
      children = box :: titleNode :: rows,
      inputCapture = InputCapture.Modal
    )

  /**
   * One row in a [[fileDialog]] / [[directoryDialog]] listing.
   *
   * The helpers are pure presentation: they don't touch the filesystem.
   * Apps build a `Seq[FileEntry]` (typically via [[listEntries]]) and pass
   * it in alongside the current selection. This keeps directory traversal
   * and any caching policy (e.g. listing in a `Future` for huge trees) out
   * of the dialog rendering, which stays trivially testable.
   */
  enum FileEntry:
    /** Sentinel for the parent directory. Rendered as `"..".`. */
    case Parent

    /** A subdirectory of the currently displayed path. */
    case Directory(name: String)

    /** A regular file in the currently displayed path. */
    case File(name: String, size: Long)

    /** True for `Parent` and `Directory(_)`; false for `File(_, _)`. */
    def isDirectory: Boolean = this match
      case Parent | Directory(_) => true
      case File(_, _)            => false

    /** Human-readable name; directories get a trailing `/`. */
    def displayName: String = this match
      case Parent          => ".."
      case Directory(name) => s"$name/"
      case File(name, _)   => name

  /**
   * Synchronously list the immediate children of `path` as a
   * `Vector[FileEntry]`, sorted directories-first then alphabetically.
   *
   * A [[FileEntry.Parent]] sentinel is prepended unless `path` is the
   * filesystem root. Hidden entries (names starting with `.`) are filtered
   * out unless `showHidden` is set.
   *
   * Errors (path not a directory, permission denied, I/O failure) are
   * surfaced as `TermFlowError.Unexpected` so the caller can choose
   * whether to render an error toast or fall back. Apps that need
   * non-blocking listings on slow filesystems should wrap the call in a
   * `Cmd.asyncResult` rather than calling this from `view`.
   *
   * @param path        Directory to list.
   * @param showHidden  Include dotfiles when true.
   */
  def listEntries(path: Path, showHidden: Boolean = false): Result[Vector[FileEntry]] =
    Try {
      if !Files.isDirectory(path) then throw new IllegalArgumentException(s"Not a directory: $path")
      val parentEntry =
        if path.getParent != null then Vector(FileEntry.Parent) else Vector.empty
      val stream = Files.newDirectoryStream(path)
      try
        val all = stream.iterator().asScala.toVector
        val visible =
          if showHidden then all
          else all.filterNot(p => Option(p.getFileName).exists(_.toString.startsWith(".")))
        val (rawDirs, rawFiles) = visible.partition(p => Files.isDirectory(p))
        val dirs =
          rawDirs
            .sortBy(p => Option(p.getFileName).fold("")(_.toString.toLowerCase))
            .map(p => FileEntry.Directory(p.getFileName.toString))
        val files =
          rawFiles
            .sortBy(p => Option(p.getFileName).fold("")(_.toString.toLowerCase))
            .map { p =>
              val size = Try(Files.size(p)).getOrElse(0L)
              FileEntry.File(p.getFileName.toString, size)
            }
        parentEntry ++ dirs ++ files
      finally stream.close()
    }.toEither.left.map(t => TermFlowError.Unexpected(s"listEntries failed for $path", Some(t)))

  /**
   * Layout descriptor for a [[fileDialog]] or [[directoryDialog]]. Mirrors
   * [[ListSelectLayout]] so apps wiring mouse hit-testing on the listing
   * portion can map a click row to an entry without re-deriving the
   * helper's anchor math.
   *
   * @param visibleCount   Number of list rows actually drawn.
   * @param anchorIndex    Index of the first visible entry — selection
   *                       scrolls relative to this.
   * @param firstRowOffset Y-offset of the first list row inside the
   *                       overlay rectangle (panel-local).
   */
  final case class FileDialogLayout(
    visibleCount: Int,
    anchorIndex: Int,
    firstRowOffset: Int
  )

  /** First selectable row inside a fileDialog (panel-local Y). */
  private val FileDialogFirstRowOffset: Int = 4

  /** Compute the visible-window math `fileDialog` uses internally. */
  def fileDialogLayout(
    entriesSize: Int,
    selectedIndex: Int,
    maxVisible: Int = 10
  ): FileDialogLayout =
    val visible = math.max(1, math.min(maxVisible, math.max(1, entriesSize)))
    val anchor =
      if entriesSize == 0 then 0
      else
        val sel    = math.max(0, math.min(entriesSize - 1, selectedIndex))
        val maxTop = math.max(0, entriesSize - visible)
        val raw    = sel - visible / 2
        math.max(0, math.min(maxTop, raw))
    FileDialogLayout(visible, anchor, FileDialogFirstRowOffset)

  /**
   * Centred file-picker dialog. Renders the current path on the second
   * row, a scrollable list of [[FileEntry]] values below it, and an
   * OK / Cancel action row at the bottom.
   *
   * Like the other helpers, `fileDialog` is pure presentation. The app
   * holds:
   *
   *   - the current `Path` and a precomputed `Seq[FileEntry]` for it
   *     (via [[listEntries]] or its own listing strategy);
   *   - a `selectedIndex` that the app advances on `↑`/`↓`;
   *   - logic that, on `Enter` over a [[FileEntry.Directory]] or
   *     [[FileEntry.Parent]], rebuilds `entries` for the new path.
   *
   * The dialog itself doesn't filter or sort entries — it draws what it
   * is given. Use [[directoryDialog]] when you want a directory-only
   * picker; in that mode entries should already be filtered to
   * directories.
   *
   * Sizing: width is the max of (title + padding), (path + padding),
   * (longest item + size column + padding), (action row + padding) with
   * a 50-cell floor. Height is `6 + visibleCount`.
   *
   * @param title         Title rendered on the top border.
   * @param currentPath   Path shown on the second row. Truncated from
   *                      the left with `…` if longer than the inner width.
   * @param entries       Listing rows to draw. The first entry is shown
   *                      at the top of the viewport; selection scrolls
   *                      the viewport per [[fileDialogLayout]].
   * @param selectedIndex Index of the highlighted row.
   * @param okFocused     Action-button focus.
   * @param maxVisible    Number of entry rows visible at once. Defaults to 10.
   * @param showSizes     Render a right-aligned size column for files.
   *                      Off for [[directoryDialog]] by default.
   * @param okLabel       Label for the confirm button.
   * @param cancelLabel   Label for the cancel button.
   * @param position      Anchor (default [[OverlayPosition.Centered]]).
   */
  def fileDialog(
    title: String,
    currentPath: Path,
    entries: Seq[FileEntry],
    selectedIndex: Int,
    okFocused: Boolean = true,
    maxVisible: Int = 10,
    showSizes: Boolean = true,
    okLabel: String = "Open",
    cancelLabel: String = "Cancel",
    position: OverlayPosition = OverlayPosition.Centered
  )(using theme: Theme): Overlay =
    val layout  = fileDialogLayout(entries.size, selectedIndex, maxVisible)
    val visible = layout.visibleCount
    val anchor  = layout.anchorIndex
    val choices = List(Choice(okLabel, okFocused), Choice(cancelLabel, !okFocused))
    // Clamp to match the scroll anchor so an out-of-range selectedIndex still
    // highlights a row instead of leaving the list visually unselected.
    val clampedSelectedIndex =
      if entries.isEmpty then -1 else math.max(0, math.min(entries.size - 1, selectedIndex))

    val pathStr    = currentPath.toString
    val titleLen   = title.length + 4
    val pathLen    = pathStr.length + 8 // "Path: " + name + padding
    val actionsLen = choiceWidth(choices) + 4
    val sizeColLen = if showSizes then 10 else 0
    val itemLen =
      (0 +: entries.map(e => e.displayName.length)).max + 4 + sizeColLen
    val width  = math.max(50, math.max(titleLen, math.max(pathLen, math.max(actionsLen, itemLen))))
    val height = 6 + visible

    val box = Theme.box(XCoord(1), YCoord(1), width, height)
    val titleNode =
      TextNode(XCoord(3), YCoord(1), List(Text(s" $title ", Style(fg = theme.primary, bold = true))))

    val innerWidth = width - 4
    val pathLabel  = "Path: "
    val pathBudget = math.max(1, innerWidth - pathLabel.length)
    val pathDisplay =
      if pathStr.length <= pathBudget then pathStr
      else "…" + pathStr.takeRight(math.max(1, pathBudget - 1))
    val pathNode =
      TextNode(
        XCoord(3),
        YCoord(2),
        List(
          Text(pathLabel, Style(fg = theme.secondary)),
          Text(pathDisplay, Style(fg = theme.foreground))
        )
      )

    val rows: List[VNode] =
      entries
        .slice(anchor, anchor + visible)
        .zipWithIndex
        .map { case (entry, i) =>
          val absIdx = anchor + i
          val sel    = absIdx == clampedSelectedIndex
          renderEntryRow(entry, sel, layout.firstRowOffset + i, innerWidth, showSizes, theme)
        }
        .toList

    val actionsRow = renderActions(choices, width, height - 1, theme)

    Overlay(
      position = position,
      width = width,
      height = height,
      children = (box :: titleNode :: pathNode :: rows) :+ actionsRow,
      inputCapture = InputCapture.Modal
    )

  /**
   * Centred directory-picker dialog. Identical layout to [[fileDialog]]
   * but defaults `okLabel = "Select"` and `showSizes = false`. Apps are
   * expected to have already filtered `entries` to directories (and the
   * [[FileEntry.Parent]] sentinel) — the dialog itself doesn't enforce
   * the constraint, so listing files alongside directories still
   * renders, just without sizes.
   */
  def directoryDialog(
    title: String,
    currentPath: Path,
    entries: Seq[FileEntry],
    selectedIndex: Int,
    okFocused: Boolean = true,
    maxVisible: Int = 10,
    okLabel: String = "Select",
    cancelLabel: String = "Cancel",
    position: OverlayPosition = OverlayPosition.Centered
  )(using Theme): Overlay =
    fileDialog(
      title = title,
      currentPath = currentPath,
      entries = entries,
      selectedIndex = selectedIndex,
      okFocused = okFocused,
      maxVisible = maxVisible,
      showSizes = false,
      okLabel = okLabel,
      cancelLabel = cancelLabel,
      position = position
    )

  /**
   * Format a byte count as a short, human-readable size string for the
   * file-dialog size column. Kept package-private for tests.
   */
  private[tui] def formatSize(bytes: Long): String =
    if bytes < 1024L then s"$bytes B"
    else
      val units         = Vector("KB", "MB", "GB", "TB", "PB")
      var value: Double = bytes.toDouble / 1024.0
      var idx           = 0
      while value >= 1024.0 && idx < units.size - 1 do
        value /= 1024.0
        idx += 1
      val rendered =
        if value >= 100.0 then f"$value%.0f"
        else if value >= 10.0 then f"$value%.1f"
        else f"$value%.2f"
      s"$rendered ${units(idx)}"

  private def renderEntryRow(
    entry: FileEntry,
    selected: Boolean,
    row: Int,
    innerWidth: Int,
    showSizes: Boolean,
    theme: Theme
  ): VNode =
    val marker = if selected then "▸ " else "  "
    val (glyph, glyphColor) = entry match
      case FileEntry.Parent       => ("↩ ", theme.info)
      case FileEntry.Directory(_) => ("▸ ", theme.primary)
      case FileEntry.File(_, _)   => ("  ", theme.foreground)
    val nameStyle =
      if selected then Style(fg = theme.background, bg = theme.primary, bold = true)
      else Style(fg = theme.foreground)
    val markerStyle =
      if selected then nameStyle
      else Style(fg = glyphColor, bold = entry.isDirectory)
    val sizeText = entry match
      case FileEntry.File(_, size) if showSizes => formatSize(size)
      case _                                    => ""
    val nameMaxLen =
      math.max(1, innerWidth - marker.length - glyph.length - (if sizeText.nonEmpty then sizeText.length + 2 else 0))
    val displayName = entry.displayName
    val truncatedName =
      if displayName.length <= nameMaxLen then displayName
      else displayName.take(math.max(1, nameMaxLen - 1)) + "…"
    val padCount =
      if sizeText.isEmpty then 0
      else math.max(1, nameMaxLen - truncatedName.length + 1)
    val padding = if padCount > 0 then " " * padCount else ""
    val sizeStyle =
      if selected then nameStyle
      else Style(fg = theme.secondary)
    val segments: List[Text] =
      val base = List(
        Text(marker, markerStyle),
        Text(glyph, markerStyle),
        Text(truncatedName, nameStyle)
      )
      if sizeText.isEmpty then base
      else base ++ List(Text(padding, nameStyle), Text(sizeText, sizeStyle))
    TextNode(XCoord(3), YCoord(row), segments)

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
