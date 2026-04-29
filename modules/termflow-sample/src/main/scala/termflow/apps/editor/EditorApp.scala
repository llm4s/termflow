package termflow.apps.editor

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Try

/**
 * Multi-buffer text editor sample (Stage 4 §3.2).
 *
 * Demonstrates `widgets.MultiLineInput` + `widgets.SplitPane` +
 * `widgets.MenuBar` working together: a menu bar across the top, a
 * buffer list on the left, and the active buffer's editor on the
 * right. Files open via a `Dialogs.textInput` overlay; quitting with
 * unsaved buffers triggers a `Dialogs.confirm` overlay.
 *
 * ## Keys (editor focus)
 *
 *   - `Ctrl+N`            — new untitled buffer
 *   - `Ctrl+O`            — open path dialog
 *   - `Ctrl+S`            — save (writes to the buffer's path; refuses
 *                           untitled buffers)
 *   - `Ctrl+W`            — close current buffer (last-buffer close
 *                           leaves an empty untitled buffer)
 *   - `Ctrl+Tab`          — next buffer
 *   - `F2`                — focus the menu bar
 *   - `q` is *not* a quit key — it's a printable character. Use
 *     `Ctrl+Q`, `Ctrl+C`, or *File → Quit*.
 *
 * ## Keys (menu focus)
 *
 *   - `←` / `→`           — switch menu
 *   - `↑` / `↓`           — move within the open menu
 *   - `Enter` / `Space`   — open / pick the cursor item
 *   - `Esc`               — return focus to the editor
 *
 * Run with `sbt editorDemo`.
 */
object EditorApp:

  // ---- Buffer / Model ------------------------------------------------------

  final case class Buffer(
    name: String,
    path: Option[Path],
    state: widgets.MultiLineInput.State,
    dirty: Boolean
  ):
    def label: String = (if dirty then "* " else "  ") + name

  enum Dialog:
    case OpenPath(buffer: String, cursor: Int)
    case ConfirmQuit(yesFocused: Boolean, dirtyCount: Int)

  enum Focus:
    case Editor
    case Menu

  final case class Model(
    width: Int,
    height: Int,
    buffers: Vector[Buffer],
    active: Int,
    menu: widgets.MenuBar.State,
    focus: Focus,
    splitRatio: Double,
    dialog: Option[Dialog],
    status: String,
    input: Sub[Msg]
  ):
    def activeBuffer: Buffer = buffers(active)

    def replaceActive(b: Buffer): Model =
      copy(buffers = buffers.updated(active, b))

  enum Msg:
    case KeyPressed(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)
    case Quit

  // ---- Menu definition -----------------------------------------------------

  val FileMenuIdx: Int = 0
  val EditMenuIdx: Int = 1

  val FileMenuItems: Vector[String] =
    Vector("New", "Open…", "Save", "Close", "Quit")
  val EditMenuItems: Vector[String] =
    Vector("Clear")

  val InitialMenuState: widgets.MenuBar.State =
    widgets.MenuBar.State(
      menus = Vector(
        widgets.MenuBar.Menu("File", FileMenuItems),
        widgets.MenuBar.Menu("Edit", EditMenuItems)
      )
    )

  // ---- Initial model -------------------------------------------------------

  def initialModel(width: Int, height: Int, input: Sub[Msg]): Model =
    Model(
      width = width,
      height = height,
      buffers = Vector(Buffer("untitled", None, widgets.MultiLineInput.State.empty, dirty = false)),
      active = 0,
      menu = InitialMenuState,
      focus = Focus.Editor,
      splitRatio = 0.25,
      dialog = None,
      status = "Ready",
      input = input
    )

  // ---- File operations -----------------------------------------------------

  private def loadFile(path: Path): Either[String, Buffer] =
    Try {
      val text  = Files.readString(path, StandardCharsets.UTF_8)
      val state = widgets.MultiLineInput.State.of(text)
      val name  = Option(path.getFileName).map(_.toString).getOrElse(path.toString)
      Buffer(name, Some(path.toAbsolutePath), state, dirty = false)
    }.toEither.left.map(e => s"open failed: ${e.getClass.getSimpleName}")

  private def saveBuffer(b: Buffer): Either[String, Buffer] =
    b.path match
      case None => Left("untitled buffer — nothing to save (open a path first)")
      case Some(p) =>
        Try {
          Files.writeString(p, b.state.text, StandardCharsets.UTF_8)
          b.copy(dirty = false)
        }.toEither.left.map(e => s"save failed: ${e.getClass.getSimpleName}")

  // ---- Buffer ops on the model --------------------------------------------

  private def newBuffer(m: Model): Model =
    val nextN = m.buffers.count(_.path.isEmpty) + 1
    val name  = if nextN == 1 then "untitled" else s"untitled-$nextN"
    val b     = Buffer(name, None, widgets.MultiLineInput.State.empty, dirty = false)
    m.copy(
      buffers = m.buffers :+ b,
      active = m.buffers.size,
      status = s"new buffer: $name",
      focus = Focus.Editor
    )

  private def closeActive(m: Model): Model =
    if m.buffers.size == 1 then
      // Replace the only buffer with a fresh untitled one.
      m.copy(
        buffers = Vector(Buffer("untitled", None, widgets.MultiLineInput.State.empty, dirty = false)),
        active = 0,
        status = "buffer closed"
      )
    else
      val remaining = m.buffers.patch(m.active, Nil, 1)
      m.copy(
        buffers = remaining,
        active = math.min(m.active, remaining.size - 1),
        status = "buffer closed"
      )

  private def nextBuffer(m: Model): Model =
    if m.buffers.size <= 1 then m
    else m.copy(active = (m.active + 1) % m.buffers.size, status = "switched buffer")

  private def saveActive(m: Model): Model =
    saveBuffer(m.activeBuffer) match
      case Right(b)  => m.replaceActive(b).copy(status = s"saved ${b.name}")
      case Left(err) => m.copy(status = err)

  private def openPath(m: Model, raw: String): Model =
    val trimmed = raw.trim
    if trimmed.isEmpty then m.copy(dialog = None, status = "open cancelled")
    else
      Try(Paths.get(trimmed)).toEither match
        case Left(_) => m.copy(dialog = None, status = s"invalid path: $trimmed")
        case Right(p) =>
          loadFile(p) match
            case Left(err) => m.copy(dialog = None, status = err)
            case Right(b) =>
              m.copy(
                buffers = m.buffers :+ b,
                active = m.buffers.size,
                dialog = None,
                status = s"opened ${b.name}",
                focus = Focus.Editor
              )

  // ---- Menu pick → action --------------------------------------------------

  /** Translate a `(menuIdx, itemIdx)` pick into the resulting model. */
  private def applyMenuPick(m: Model, menuIdx: Int, itemIdx: Int): Model =
    (menuIdx, itemIdx) match
      case (FileMenuIdx, 0) => newBuffer(m).copy(focus = Focus.Editor)
      case (FileMenuIdx, 1) => m.copy(dialog = Some(Dialog.OpenPath("", 0)), focus = Focus.Editor)
      case (FileMenuIdx, 2) => saveActive(m).copy(focus = Focus.Editor)
      case (FileMenuIdx, 3) => closeActive(m).copy(focus = Focus.Editor)
      case (FileMenuIdx, 4) => requestQuit(m)
      case (EditMenuIdx, 0) =>
        val cleared = m.activeBuffer.copy(state = widgets.MultiLineInput.State.empty, dirty = true)
        m.replaceActive(cleared).copy(focus = Focus.Editor, status = "cleared")
      case _ => m.copy(focus = Focus.Editor)

  private def requestQuit(m: Model): Model =
    val dirty = m.buffers.count(_.dirty)
    if dirty == 0 then m.copy(status = "quit")
    else m.copy(dialog = Some(Dialog.ConfirmQuit(yesFocused = false, dirtyCount = dirty)), focus = Focus.Editor)

  // ---- Key dispatch --------------------------------------------------------

  /**
   * Pure step. Returns either a new model, or a marker that the caller
   * should exit the runtime. Keeps `update` thin.
   */
  enum StepResult:
    case StayInModel(model: Model)
    case ExitNow(model: Model)

  def step(m: Model, msg: Msg): StepResult =
    msg match
      case Msg.Quit        => StepResult.ExitNow(m)
      case Msg.KeyError(_) => StepResult.StayInModel(m)
      case Msg.KeyPressed(k) =>
        m.dialog match
          case Some(d) => StepResult.StayInModel(handleDialogKey(m, d, k))
          case None    => handleNormalKey(m, k)

  private def handleDialogKey(m: Model, d: Dialog, k: KeyDecoder.InputKey): Model =
    import KeyDecoder.InputKey.*
    d match
      case Dialog.OpenPath(buf, cursor) =>
        k match
          case Enter =>
            openPath(m, buf)
          case Escape =>
            m.copy(dialog = None, status = "open cancelled")
          case Backspace =>
            if cursor == 0 then m
            else
              val newBuf = buf.substring(0, cursor - 1) + buf.substring(cursor)
              m.copy(dialog = Some(Dialog.OpenPath(newBuf, cursor - 1)))
          case CharKey(ch) =>
            val newBuf = buf.substring(0, cursor) + ch + buf.substring(cursor)
            m.copy(dialog = Some(Dialog.OpenPath(newBuf, cursor + 1)))
          case ArrowLeft =>
            m.copy(dialog = Some(Dialog.OpenPath(buf, math.max(0, cursor - 1))))
          case ArrowRight =>
            m.copy(dialog = Some(Dialog.OpenPath(buf, math.min(buf.length, cursor + 1))))
          case Home =>
            m.copy(dialog = Some(Dialog.OpenPath(buf, 0)))
          case End =>
            m.copy(dialog = Some(Dialog.OpenPath(buf, buf.length)))
          case _ => m

      case Dialog.ConfirmQuit(yesFocused, count) =>
        k match
          case ArrowLeft | ArrowRight | Tab | BackTab =>
            m.copy(dialog = Some(Dialog.ConfirmQuit(!yesFocused, count)))
          case Enter | CharKey(' ') =>
            if yesFocused then m.copy(dialog = None, status = "quit")
            else m.copy(dialog = None, status = "stay")
          case Escape =>
            m.copy(dialog = None, status = "stay")
          case _ => m

  private def handleNormalKey(m: Model, k: KeyDecoder.InputKey): StepResult =
    import KeyDecoder.InputKey.*
    k match
      // Quit shortcuts.
      case Ctrl('Q') | Ctrl('C') =>
        val nm = requestQuit(m)
        if nm.dialog.isEmpty then StepResult.ExitNow(nm) else StepResult.StayInModel(nm)

      // Confirm-quit follow-up: if the dialog committed "yes", status = "quit".
      // We don't reach this branch from inside the dialog itself — the dialog
      // routes back here only after closing.
      // Buffer + IO shortcuts.
      case Ctrl('N') => StepResult.StayInModel(newBuffer(m))
      case Ctrl('O') => StepResult.StayInModel(m.copy(dialog = Some(Dialog.OpenPath("", 0))))
      case Ctrl('S') => StepResult.StayInModel(saveActive(m))
      case Ctrl('W') => StepResult.StayInModel(closeActive(m))
      case Ctrl('I') => StepResult.StayInModel(nextBuffer(m)) // Ctrl-Tab on most terminals
      case F2        => StepResult.StayInModel(m.copy(focus = Focus.Menu))
      case _ =>
        m.focus match
          case Focus.Menu =>
            k match
              case Escape => StepResult.StayInModel(m.copy(focus = Focus.Editor))
              case _ =>
                val r         = widgets.MenuBar.handleKey(m.menu, k)
                val withState = m.copy(menu = r.state)
                r.picked match
                  case Some((mi, ii)) => StepResult.StayInModel(applyMenuPick(withState, mi, ii))
                  case None           => StepResult.StayInModel(withState)
          case Focus.Editor =>
            // Forward to MultiLineInput.
            val (nextState, _) = widgets.MultiLineInput.handleKey[Msg](m.activeBuffer.state, k)
            val nextBuf        = m.activeBuffer.copy(state = nextState, dirty = true)
            StepResult.StayInModel(m.replaceActive(nextBuf))

  // ---- View ---------------------------------------------------------------

  private def view(m: Model): RootNode =
    given Theme = Theme.dark
    val w       = math.max(60, m.width)
    val h       = math.max(15, m.height)

    // Menu bar at row 1.
    val menuNodes = widgets.MenuBar(
      m.menu,
      at = Coord(1.x, 1.y),
      focused = m.focus == Focus.Menu
    )

    // Split pane occupies rows 2 .. (h - 1). Row h is status bar.
    val splitTop    = 2
    val splitHeight = math.max(3, h - 2 - 1)

    val statusBar = widgets.StatusBar(
      left = s" buf ${m.active + 1}/${m.buffers.size} · ${m.activeBuffer.label.trim} ",
      center = s" ${m.status} ",
      right = " Ctrl+Q quit ",
      width = w,
      at = Coord(1.x, h.y)
    )

    val splitNodes = widgets.SplitPane(
      first = (origin: Coord, fw: Int, fh: Int) => bufferList(m, origin, fw, fh),
      second = (origin: Coord, fw: Int, fh: Int) => editorPane(m, origin, fw, fh),
      width = w,
      height = splitHeight,
      direction = widgets.SplitPane.Direction.Horizontal,
      at = Coord(1.x, splitTop.y),
      splitRatio = m.splitRatio,
      gap = 1
    )

    // Overlay (textInput / confirm) renders above everything else.
    val overlay: List[Overlay] = m.dialog match
      case None => Nil
      case Some(Dialog.OpenPath(buf, cur)) =>
        List(
          Dialogs.textInput(
            title = "Open file",
            prompt = "Path:",
            value = buf,
            cursor = cur,
            okFocused = true
          )
        )
      case Some(Dialog.ConfirmQuit(yes, count)) =>
        List(
          Dialogs.confirm(
            prompt = s"$count buffer${if count == 1 then "" else "s"} unsaved. Quit anyway?",
            yesFocused = yes,
            title = "Quit"
          )
        )

    RootNode(
      width = w,
      height = h,
      children = menuNodes ++ splitNodes :+ statusBar,
      input = None,
      overlays = overlay
    )

  /** Left pane: list of buffers. */
  private def bufferList(m: Model, origin: Coord, w: Int, h: Int)(using theme: Theme): List[VNode] =
    val title = TextNode(
      origin.x,
      origin.y,
      List(Text("Buffers", Style(fg = theme.primary, bold = true)))
    )
    val rows = m.buffers.zipWithIndex.toList.map { case (b, idx) =>
      val style =
        if idx == m.active then Style(fg = theme.background, bg = theme.primary, bold = true)
        else Style(fg = theme.foreground)
      val text = b.label.padTo(math.max(8, w - 1), ' ').take(math.max(1, w - 1))
      TextNode(origin.x, origin.y + 1 + idx, List(Text(text, style)))
    }
    val _ = h
    title :: rows

  /** Right pane: the active MultiLineInput. */
  private def editorPane(m: Model, origin: Coord, w: Int, h: Int)(using theme: Theme): List[VNode] =
    val header = TextNode(
      origin.x,
      origin.y,
      List(
        Text(m.activeBuffer.label.trim, Style(fg = theme.primary, bold = true)),
        " — ".text(fg = theme.secondary),
        Text(
          m.activeBuffer.path.map(_.toString).getOrElse("(unsaved)"),
          Style(fg = theme.secondary, italic = true)
        )
      )
    )
    val body = widgets.MultiLineInput.render(
      m.activeBuffer.state,
      width = math.max(1, w),
      height = math.max(1, h - 1),
      at = Coord(origin.x, origin.y + 1)
    )
    header :: body

  // ---- App ----------------------------------------------------------------

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val keys = Sub.InputKey[Msg](
        msg = Msg.KeyPressed.apply,
        onError = Msg.KeyError.apply,
        ctx = ctx
      )
      initialModel(ctx.terminal.width, ctx.terminal.height, keys).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = m.copy(width = ctx.terminal.width, height = ctx.terminal.height)
      step(sized, msg) match
        case StepResult.StayInModel(next) => next.tui
        case StepResult.ExitNow(next)     => Tui(next, Cmd.Exit)

    override def view(m: Model): RootNode = EditorApp.view(m)

    override def toMsg(input: PromptLine): Result[Msg] =
      val _ = input
      Left(TermFlowError.Validation("EditorApp has no prompt"))

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)
