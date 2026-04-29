# Open a file picker and load the result

`Dialogs.fileDialog` is a presentational overlay — like
`Dialogs.confirm`, it returns an `Overlay` and doesn't take
callbacks. The app owns the current path, the directory listing, the
selected index, and rebuilds the listing when the user steps into a
subdirectory.

## Pattern

1. Hold a `picker: Option[PickerState]` on the model with the path,
   entries, and selected index.
2. On `Open`, list the start directory and store it in the picker.
3. Route keys to the picker while it's open — Enter dives into a
   directory or returns the selected file; Esc cancels.
4. On commit, close the picker and dispatch a domain `Msg` carrying
   the chosen path.

## Code

```scala
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

import termflow.tui.{Dialogs, FileEntry, Theme}
import termflow.tui.KeyDecoder.InputKey.*

final case class PickerState(
  path:          Path,
  entries:       Seq[FileEntry],
  selectedIndex: Int,
  okFocused:     Boolean = true
)

final case class Model(
  loadedFile: Option[Path],
  picker:     Option[PickerState]
)

enum Msg:
  case OpenPicker
  case PickerKey(k: InputKey)
  case FileChosen(p: Path)
  case PickerCancelled

def listDir(p: Path): Seq[FileEntry] =
  // FileEntry has fields name, path, isDir, sizeBytes — see
  // termflow.tui.FileEntry. Skipping sort/hidden filtering for brevity.
  Files.list(p).iterator.asScala.toList.map(FileEntry.fromPath).sortBy(e => (!e.isDir, e.name))

def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Msg.OpenPicker =>
      val cwd = Path.of(System.getProperty("user.dir"))
      m.copy(picker = Some(PickerState(cwd, listDir(cwd), 0))).tui

    case Msg.PickerKey(k) =>
      m.picker match
        case None => m.tui
        case Some(p) =>
          k match
            case ArrowDown =>
              val next = math.min(p.entries.size - 1, p.selectedIndex + 1)
              m.copy(picker = Some(p.copy(selectedIndex = next))).tui

            case ArrowUp =>
              val next = math.max(0, p.selectedIndex - 1)
              m.copy(picker = Some(p.copy(selectedIndex = next))).tui

            case Enter | CharKey(' ') =>
              p.entries.lift(p.selectedIndex) match
                case Some(entry) if entry.isDir =>
                  // Step in: rebuild listing
                  m.copy(picker = Some(p.copy(
                    path          = entry.path,
                    entries       = listDir(entry.path),
                    selectedIndex = 0
                  ))).tui
                case Some(entry) =>
                  m.copy(picker = None).gCmd(Msg.FileChosen(entry.path))
                case None => m.tui

            case Escape =>
              m.copy(picker = None).gCmd(Msg.PickerCancelled)

            case _ => m.tui

    case Msg.FileChosen(p) =>
      m.copy(loadedFile = Some(p)).tui

    case Msg.PickerCancelled =>
      m.tui

def view(m: Model): RootNode =
  given Theme = Theme.dark
  val baseChildren = /* … */
  val overlays = m.picker.toList.map { p =>
    Dialogs.fileDialog(
      title         = "Open file",
      currentPath   = p.path,
      entries       = p.entries,
      selectedIndex = p.selectedIndex,
      okFocused     = p.okFocused,
      maxVisible    = 12,
      showSizes     = true
    )
  }
  RootNode(80, 24, children = baseChildren, input = None, overlays = overlays)
```

## Notes

- **`FileEntry`** is a small case class shipped in `termflow.tui`
  with `name`, `path`, `isDir`, `sizeBytes`. Build your own list any
  way you like — flat directory listing, recursive search,
  pre-filtered globs. The dialog only renders.
- **Hidden / dotfiles.** Filter at the `listDir` level so the dialog
  never shows them. Adding a `showHidden: Boolean` to `PickerState`
  is straightforward.
- **Read the file inside `FileChosen`** if it's small. For larger
  loads, return `Cmd.FCmd(Future { … }, Msg.LoadDone.apply)` and
  treat it as async work — the
  [Async tutorial](../tut/03-async.md) covers the pattern.
- **Symlink loops** are not detected by `listDir` above — guard with
  `Files.readAttributes(..., LinkOption.NOFOLLOW_LINKS)` if you walk
  user-supplied trees.

For a working example, see
[`apps.dialog.FileDialogDemoApp`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/dialog/FileDialogDemoApp.scala).
