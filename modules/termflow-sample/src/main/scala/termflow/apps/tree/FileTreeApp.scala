package termflow.apps.tree

import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets.Tree

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.Using

/**
 * Two-pane file-tree explorer (Stage 4 §3.2 sample).
 *
 * Left pane: a [[widgets.Tree]] of the working directory. Directories
 * expand on demand; their listings are cached on the model so re-expanding
 * is free. Right pane: details about the selected entry — absolute path,
 * size, kind (file / dir / symlink), and (for files under 2 KiB) a small
 * preview of the first few lines.
 *
 * Demonstrates: lazy directory traversal under the [[Tree.Children]]
 * typeclass, [[Layout.Border]] for the four-pane shell (header + left
 * tree + center details + bottom status), [[Layout.Zone]] + hit-test
 * passthrough for chevron-vs-label mouse clicks, and `Sub.InputKey`
 * auto-registration via `RuntimeCtx`.
 *
 * ## Keys
 *
 *   - `↑` / `↓`     — move selection
 *   - `Enter` / `Space` / `→` — toggle expand on a directory; on a file,
 *                              this is a no-op (the details pane already
 *                              shows the preview).
 *   - `←`            — collapse the directory under the cursor, or jump
 *                       to its parent if it's already collapsed.
 *   - `q` / `Esc` / `Ctrl+C`  quit.
 *
 * ## Mouse
 *
 *   - Click chevron — toggle expand.
 *   - Click label — select.
 *
 * Run with `sbt treeDemo`.
 */
object FileTreeApp:

  // ---- Domain --------------------------------------------------------------

  /** A node in the visible tree (only nodes inside expanded dirs appear). */
  final case class FsNode(path: Path, isDir: Boolean, children: Vector[FsNode])

  given Tree.Children[FsNode, String] with
    def id(n: FsNode): String           = n.path.toAbsolutePath.toString
    def kids(n: FsNode): Vector[FsNode] = n.children

  // ---- Model + Msg ---------------------------------------------------------

  enum DetailsPreview:
    case None
    case Lines(lines: Vector[String])
    case Truncated(reason: String)

  final case class Details(
    path: Path,
    isDir: Boolean,
    sizeBytes: Long,
    preview: DetailsPreview
  )

  final case class Model(
    root: Path,
    expanded: Set[String],
    childrenCache: Map[String, Vector[FsNode]],
    selectedIndex: Int,
    width: Int,
    height: Int,
    input: Sub[Msg]
  ):
    /** The visible tree built from the current expanded set + cache. */
    def visibleTree: FsNode = buildVisibleTree(root, expanded, childrenCache)

    def visibleRows: Vector[Tree.Row[FsNode]] =
      Tree.visibleRows(Vector(visibleTree), expanded)

    def selectedNode: Option[FsNode] =
      visibleRows.lift(selectedIndex).map(_.node)

  enum Msg:
    case KeyPressed(k: KeyDecoder.InputKey)
    case Quit
    case KeyError(t: Throwable)

  // ---- Tree construction ---------------------------------------------------

  private def listDir(p: Path): Vector[FsNode] =
    val attempt = Using(Files.list(p))(stream => stream.iterator.asScala.toVector)
    attempt.toOption
      .getOrElse(Vector.empty)
      .map(child => FsNode(child, Files.isDirectory(child), Vector.empty))
      .sortBy(n => (!n.isDir, n.path.getFileName.toString.toLowerCase))

  private def buildVisibleTree(
    p: Path,
    expanded: Set[String],
    cache: Map[String, Vector[FsNode]]
  ): FsNode =
    val isDir = Files.isDirectory(p)
    val key   = p.toAbsolutePath.toString
    val kids =
      if isDir && expanded.contains(key) then
        cache
          .getOrElse(key, Vector.empty)
          .map(child => buildVisibleTree(child.path, expanded, cache))
      else Vector.empty
    FsNode(p, isDir, kids)

  /** Ensure `model.childrenCache` has an entry for `path`, listing if not. */
  private def withListing(model: Model, path: Path): Model =
    val key = path.toAbsolutePath.toString
    if model.childrenCache.contains(key) then model
    else model.copy(childrenCache = model.childrenCache.updated(key, listDir(path)))

  // ---- Update --------------------------------------------------------------

  private def toggleExpand(m: Model): Model =
    m.selectedNode match
      case Some(node) if node.isDir =>
        val key       = node.path.toAbsolutePath.toString
        val withCache = withListing(m, node.path)
        if m.expanded.contains(key) then withCache.copy(expanded = m.expanded - key)
        else withCache.copy(expanded = m.expanded + key)
      case _ => m

  private def collapseOrAscend(m: Model): Model =
    val rows = m.visibleRows
    if rows.isEmpty then m
    else
      val row = rows(m.selectedIndex)
      val key = row.id
      if row.expanded then m.copy(expanded = m.expanded - key)
      else
        // Move selection to the parent row, if any.
        val parent = parentRowIndex(rows, m.selectedIndex)
        m.copy(selectedIndex = parent.getOrElse(m.selectedIndex))

  private def parentRowIndex(rows: Vector[Tree.Row[FsNode]], idx: Int): Option[Int] =
    if idx <= 0 then None
    else
      val depth = rows(idx).depth
      if depth == 0 then None
      else (idx - 1 to 0 by -1).find(i => rows(i).depth < depth)

  private def moveCursor(m: Model, delta: Int): Model =
    val n = m.visibleRows.size
    if n == 0 then m
    else
      val next = math.max(0, math.min(n - 1, m.selectedIndex + delta))
      m.copy(selectedIndex = next)

  // ---- Mouse helpers -------------------------------------------------------

  /**
   * Origin of the tree column inside the rendered frame. Mirrors the
   * Layout.Border / Row positions used in `view`.
   */
  private[tree] val treeOrigin: Coord = Coord(2.x, 4.y)
  private[tree] val indentWidth: Int  = 2

  private def routeMouse(m: Model, e: MouseEvent): Model = e match
    case MouseEvent.Press(_, col, row, _) =>
      val rows = m.visibleRows
      Tree.hitTest(rows, treeOrigin, indentWidth, col, row, labelLength = 64) match
        case Some(Tree.HitResult.Chevron(idx)) =>
          val target = rows(idx).node
          val key    = target.path.toAbsolutePath.toString
          val sel    = m.copy(selectedIndex = idx)
          if target.isDir then toggleExpand(sel) else sel
        case Some(Tree.HitResult.Label(idx)) =>
          m.copy(selectedIndex = idx)
        case None => m
    case _ => m

  // ---- Pure step (testable without the runtime) ----------------------------

  def step(m: Model, msg: Msg): Model =
    import KeyDecoder.InputKey.*
    msg match
      case Msg.Quit        => m
      case Msg.KeyError(_) => m
      case Msg.KeyPressed(k) =>
        k match
          case ArrowDown                         => moveCursor(m, +1)
          case ArrowUp                           => moveCursor(m, -1)
          case Enter | CharKey(' ') | ArrowRight => toggleExpand(m)
          case ArrowLeft                         => collapseOrAscend(m)
          case Mouse(e)                          => routeMouse(m, e)
          case _                                 => m

  // ---- View ---------------------------------------------------------------

  private def details(m: Model): Option[Details] =
    m.selectedNode.map { node =>
      val isDir = node.isDir
      val size  = if isDir then 0L else Try(Files.size(node.path)).getOrElse(0L)
      val preview =
        if isDir then DetailsPreview.None
        else if size > 2 * 1024 then DetailsPreview.Truncated(s"${size} bytes — preview skipped")
        else
          val attempt = Try {
            val all = Files.readAllLines(node.path).asScala.toVector
            all.take(20)
          }
          attempt.toEither match
            case Right(lines) => DetailsPreview.Lines(lines)
            case Left(err)    => DetailsPreview.Truncated(s"unreadable: ${err.getClass.getSimpleName}")
      Details(node.path, isDir, size, preview)
    }

  private def viewRender(m: Model): RootNode =
    given Theme = Theme.dark
    val w       = math.max(40, m.width)
    val h       = math.max(10, m.height)

    val title = TextNode(
      2.x,
      1.y,
      List(
        Text("File tree explorer", Style(fg = Theme.dark.primary, bold = true))
      )
    )
    val help = TextNode(
      2.x,
      2.y,
      List(
        " ↑/↓ ".themed(_.primary),
        "move  ".text,
        " Enter ".themed(_.primary),
        "expand  ".text,
        " ← ".themed(_.primary),
        "collapse  ".text,
        " q ".themed(_.primary),
        "quit".text
      )
    )

    val rows = m.visibleRows
    val treeNodes: List[VNode] = Tree(
      roots = Vector(m.visibleTree),
      expanded = m.expanded,
      selectedIndex = m.selectedIndex,
      render = (n: FsNode) => labelFor(n, m.root),
      at = treeOrigin,
      indentWidth = indentWidth,
      unicode = true
    )

    val detailsLines: List[VNode] = details(m) match
      case None =>
        List(
          TextNode(1.x, 1.y, List("(empty)".text(fg = Theme.dark.secondary)))
        )
      case Some(d) =>
        val header = TextNode(
          1.x,
          1.y,
          List(
            (if d.isDir then "📁 " else "📄 ").text,
            Text(d.path.toAbsolutePath.toString, Style(bold = true, fg = Theme.dark.primary))
          )
        )
        val sizeLine =
          if d.isDir then TextNode(1.x, 2.y, List(s"directory".text(fg = Theme.dark.info)))
          else TextNode(1.x, 2.y, List(s"size: ${d.sizeBytes} bytes".text(fg = Theme.dark.info)))
        val previewLines: List[VNode] = d.preview match
          case DetailsPreview.None => Nil
          case DetailsPreview.Truncated(why) =>
            List(TextNode(1.x, 4.y, List(why.text(fg = Theme.dark.secondary))))
          case DetailsPreview.Lines(lines) =>
            lines.zipWithIndex.toList.map { case (line, i) =>
              TextNode(1.x, (4 + i).y, List(line.take(120).text))
            }
        header :: sizeLine :: previewLines

    // Flatten the tree column and the details column into Layout.Elem stacks
    // wrapped in a Border. The header lives outside the border so it's not
    // clipped by the band sizing.
    val tw      = math.max(20, w * 2 / 5)
    val treeCol = Layout.Column(gap = 0, children = treeNodes.map(Layout.Elem.apply))
    val detsCol = Layout.Column(gap = 0, children = detailsLines.map(Layout.Elem.apply))
    val statusLine = TextNode(
      1.x,
      1.y,
      List(
        s"${rows.size} rows · ${m.expanded.size} expanded"
          .text(fg = Theme.dark.secondary)
      )
    )

    val body = Layout.border(
      left = treeCol,
      center = detsCol,
      bottom = Layout.Elem(statusLine),
      gap = 1
    )
    // Reserve the top two rows for title + help; body fills the rest.
    val resolvedBody = Layout.resolveTo(
      body,
      at = Coord(2.x, 4.y),
      availableWidth = w - 2,
      availableHeight = h - 4
    )

    RootNode(
      width = w,
      height = h,
      children = title :: help :: resolvedBody,
      input = None
    )

  private def labelFor(n: FsNode, root: Path): String =
    val name = Option(n.path.getFileName).map(_.toString).getOrElse(n.path.toString)
    val displayName =
      if n.path == root then
        // Root node: show absolute root path so the user knows where they are.
        n.path.toAbsolutePath.toString
      else name
    if n.isDir then s"$displayName/" else displayName

  // ---- Initial model -------------------------------------------------------

  /**
   * Pure factory used both by the runtime and by tests. Tests can override
   * the start path; runtime starts at the user's CWD.
   */
  def initialModel(start: Path, width: Int, height: Int, input: Sub[Msg]): Model =
    val rootKey     = start.toAbsolutePath.toString
    val rootListing = listDir(start)
    Model(
      root = start,
      expanded = Set(rootKey),
      childrenCache = Map(rootKey -> rootListing),
      selectedIndex = 0,
      width = width,
      height = height,
      input = input
    )

  // ---- App -----------------------------------------------------------------

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val keys = Sub.InputKey[Msg](
        msg = Msg.KeyPressed.apply,
        onError = Msg.KeyError.apply,
        ctx = ctx
      )
      val cwd = Paths.get(System.getProperty("user.dir"))
      initialModel(cwd, ctx.terminal.width, ctx.terminal.height, keys).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = m.copy(width = ctx.terminal.width, height = ctx.terminal.height)
      msg match
        case Msg.Quit        => Tui(sized, Cmd.Exit)
        case Msg.KeyError(_) => sized.tui
        case Msg.KeyPressed(k) =>
          import KeyDecoder.InputKey.*
          val isQuit = k match
            case CharKey('q') | CharKey('Q') | Escape | Ctrl('C') => true
            case _                                                => false
          if isQuit then Tui(sized, Cmd.Exit)
          else step(sized, msg).tui

    override def view(m: Model): RootNode = viewRender(m)

    override def toMsg(input: PromptLine): Result[Msg] =
      val _ = input
      Left(TermFlowError.Validation("FileTreeApp has no prompt"))

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)
