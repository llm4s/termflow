package termflow.apps.dialog

import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

import java.nio.file.Path
import java.nio.file.Paths

/**
 * Demonstrates the file-picker overlays shipped with Stage 3 §6.1.
 *
 * Press `f` to open `Dialogs.fileDialog`; `d` for `Dialogs.directoryDialog`.
 * Inside a dialog: arrow keys move the selection, Enter activates a row
 * (Parent / Directory navigates; File / Select returns), Esc cancels.
 */
object FileDialogDemoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  private given Theme = Theme.rounded

  enum Mode:
    case Files
    case Directories

  enum Dialog:
    case None
    case Open(
      mode: Mode,
      currentPath: Path,
      entries: Vector[Dialogs.FileEntry],
      selectedIndex: Int
    )

  final case class Model(
    lastPick: Option[String],
    dialog: Dialog,
    input: Sub[Msg]
  )

  enum Msg:
    case OpenFiles
    case OpenDirs
    case CloseDialog
    case Move(delta: Int)
    case Activate
    case Pick(path: String)
    case Refresh(currentPath: Path, entries: Vector[Dialogs.FileEntry])
    case ShowError(message: String)
    case Quit
    case Key(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:

    private val startPath: Path = Paths.get(sys.props.getOrElse("user.home", "."))

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        lastPick = None,
        dialog = Dialog.None,
        input = Sub.InputKey(k => Key(k), e => KeyError(e), ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case OpenFiles =>
          openDialog(m, Mode.Files, startPath)

        case OpenDirs =>
          openDialog(m, Mode.Directories, startPath)

        case CloseDialog =>
          m.copy(dialog = Dialog.None).tui

        case Move(delta) =>
          m.dialog match
            case Dialog.Open(mode, path, entries, idx) if entries.nonEmpty =>
              val next = math.max(0, math.min(entries.size - 1, idx + delta))
              m.copy(dialog = Dialog.Open(mode, path, entries, next)).tui
            case _ => m.tui

        case Activate =>
          m.dialog match
            case Dialog.Open(mode, path, entries, idx) if entries.nonEmpty =>
              entries(idx) match
                case Dialogs.FileEntry.Parent =>
                  val parent = Option(path.getParent).getOrElse(path)
                  refresh(m, mode, parent)
                case Dialogs.FileEntry.Directory(name) =>
                  refresh(m, mode, path.resolve(name))
                case Dialogs.FileEntry.File(name, _) =>
                  mode match
                    case Mode.Files =>
                      Tui(m.copy(dialog = Dialog.None), Cmd.GCmd(Pick(path.resolve(name).toString)))
                    case Mode.Directories => m.tui
            case _ => m.tui

        case Refresh(path, entries) =>
          m.dialog match
            case Dialog.Open(mode, _, _, _) =>
              m.copy(dialog = Dialog.Open(mode, path, entries, 0)).tui
            case _ => m.tui

        case Pick(p) =>
          m.copy(lastPick = Some(p)).tui

        case ShowError(message) =>
          m.copy(lastPick = Some(s"error: $message"), dialog = Dialog.None).tui

        case Quit        => Tui(m, Cmd.Exit)
        case KeyError(_) => m.tui
        case Key(k)      => Tui(m, dispatch(m, k))

    private def openDialog(m: Model, mode: Mode, path: Path): Tui[Model, Msg] =
      Dialogs.listEntries(path) match
        case Right(entries) =>
          val filtered = mode match
            case Mode.Files       => entries
            case Mode.Directories => entries.filter(_.isDirectory)
          m.copy(dialog = Dialog.Open(mode, path, filtered, 0)).tui
        case Left(err) =>
          m.copy(lastPick = Some(s"error: ${err.toString}")).tui

    private def refresh(m: Model, mode: Mode, path: Path): Tui[Model, Msg] =
      Dialogs.listEntries(path) match
        case Right(entries) =>
          val filtered = mode match
            case Mode.Files       => entries
            case Mode.Directories => entries.filter(_.isDirectory)
          m.copy(dialog = Dialog.Open(mode, path, filtered, 0)).tui
        case Left(err) =>
          Tui(m, Cmd.GCmd(ShowError(err.toString)))

    private def dispatch(m: Model, k: KeyDecoder.InputKey): Cmd[Msg] =
      import KeyDecoder.InputKey.*
      m.dialog match
        case Dialog.Open(_, _, _, _) =>
          k match
            case ArrowUp   => Cmd.GCmd(Move(-1))
            case ArrowDown => Cmd.GCmd(Move(+1))
            case PageUp    => Cmd.GCmd(Move(-5))
            case PageDown  => Cmd.GCmd(Move(+5))
            case Enter     => Cmd.GCmd(Activate)
            case Escape    => Cmd.GCmd(CloseDialog)
            case _         => Cmd.NoCmd
        case Dialog.None =>
          k match
            case CharKey('f') | CharKey('F')          => Cmd.GCmd(OpenFiles)
            case CharKey('d') | CharKey('D')          => Cmd.GCmd(OpenDirs)
            case CharKey('q') | CharKey('Q') | Escape => Cmd.GCmd(Quit)
            case _                                    => Cmd.NoCmd

    override def view(m: Model): RootNode =
      val title = TextNode(2.x, 1.y, List("File-dialog demo".themed(_.primary)))
      val help1 = TextNode(2.x, 3.y, List("f : open file picker    d : open directory picker".text))
      val help2 = TextNode(2.x, 4.y, List("↑/↓ : move    Enter : activate    Esc : cancel    q : quit".text))
      val pick = m.lastPick match
        case Some(p) => TextNode(2.x, 6.y, List("Last pick: ".text, p.themed(_.success)))
        case None    => TextNode(2.x, 6.y, List("Last pick: ".text, "(none yet)".themed(_.secondary)))

      val baseRoot = RootNode(
        width = 100,
        height = 16,
        children = List(title, help1, help2, pick),
        input = None
      )

      m.dialog match
        case Dialog.None => baseRoot
        case Dialog.Open(Mode.Files, path, entries, idx) =>
          baseRoot.copy(overlays = List(Dialogs.fileDialog("Open file", path, entries, idx, maxVisible = 8)))
        case Dialog.Open(Mode.Directories, path, entries, idx) =>
          baseRoot.copy(overlays =
            List(Dialogs.directoryDialog("Choose directory", path, entries, idx, maxVisible = 8))
          )

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation("FileDialogDemo has no prompt"))
