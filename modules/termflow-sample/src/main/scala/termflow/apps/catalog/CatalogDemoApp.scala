package termflow.apps.catalog

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets.*

/**
 * Showcase for the stateful widgets: [[TextField]] + [[Select]] +
 * [[ListView]] + [[Table]] composed into a tiny task manager.
 *
 * Flow:
 *   1. Type a new task in the text field
 *   2. Pick its priority from the dropdown
 *   3. Press `Add` (or hit Enter in the field) to push it onto the list
 *   4. Navigate the list with arrow keys; Enter removes the selected task
 *   5. The summary table on the right updates live as tasks are added /
 *      removed
 *
 * ## Keys
 *
 *   - `Tab` / `Shift+Tab`                cycle focus forward / backward
 *   - `Enter` (in the task field)        add the task with the selected priority
 *   - `Enter` / `Space` (on a button)    activate Add / Clear
 *   - `Enter` / `Space` (on the Select)  open/commit the dropdown
 *   - `Enter` / `Space` (on a list row)  remove the selected task
 *   - `↑` / `↓` (in List / open Select)  navigate the items
 *   - `↑` / `↓` (elsewhere)              same as Shift+Tab / Tab
 *   - `←` / `→` (on a button)            previous / next focus
 *   - `Ctrl+T`                           toggle dark / light theme
 *   - `Ctrl+C` / `Esc` / `q` (on a button) quit
 *
 * Run with:
 * {{{
 *   sbt catalogDemo
 *   // or:
 *   sbt "termflowSample/runMain termflow.apps.catalog.CatalogDemoApp"
 * }}}
 */
object CatalogDemoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  /** Priority level for a task. */
  enum Priority:
    case High, Medium, Low
    override def toString: String = this match
      case High   => "high"
      case Medium => "medium"
      case Low    => "low"

  /** One task in the catalog. */
  final case class Task(title: String, priority: Priority)

  // Focus identifiers.
  val TaskFieldId: FocusId = FocusId("task-field")
  val PriorityId: FocusId  = FocusId("priority")
  val AddId: FocusId       = FocusId("add")
  val ClearId: FocusId     = FocusId("clear")
  val ListId: FocusId      = FocusId("list")

  val FocusOrder: Vector[FocusId] = Vector(TaskFieldId, PriorityId, AddId, ClearId, ListId)

  private val Priorities = Vector(Priority.High, Priority.Medium, Priority.Low)

  /** Demo seeds — gives the user something to see on first load. */
  private val SeedTasks = Vector(
    Task("buy groceries", Priority.Medium),
    Task("write report", Priority.High),
    Task("review PR", Priority.Low),
    Task("call alice", Priority.High)
  )

  enum Msg:
    case NextFocus
    case PrevFocus
    case ToggleTheme
    case AddTask
    case ClearAll
    case RemoveSelected
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)
    case Quit

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    newTask: TextField.State,
    priority: Select.State[Priority],
    tasks: ListView.State[Task],
    fm: FocusManager,
    darkTheme: Boolean
  )

  import Msg.*

  /**
   * True globals — safe everywhere, never consumed by a widget.
   * Note: vertical arrows are deliberately NOT in Globals — the list and
   * the open Select consume them for internal navigation.
   */
  val Globals: Keymap[Msg] =
    Keymap(
      KeyDecoder.InputKey.Ctrl('C') -> Quit,
      KeyDecoder.InputKey.Escape    -> Quit,
      KeyDecoder.InputKey.Ctrl('T') -> ToggleTheme
    ) ++
      Keymap.focus(next = NextFocus, previous = PrevFocus)

  /** Letter shortcuts that only fire when focus isn't in the text field. */
  val NonTextShortcuts: Keymap[Msg] =
    Keymap(
      KeyDecoder.InputKey.CharKey('t') -> ToggleTheme,
      KeyDecoder.InputKey.CharKey('T') -> ToggleTheme,
      KeyDecoder.InputKey.CharKey('q') -> Quit,
      KeyDecoder.InputKey.CharKey('Q') -> Quit
    ) ++ Keymap.focusHorizontal(previous = PrevFocus, next = NextFocus)

  /** Arrow-key focus nav used by TextField + buttons (but NOT by List/open Select). */
  private val ArrowFocusNav: Keymap[Msg] =
    Keymap.focusVertical(previous = PrevFocus, next = NextFocus)

  /** Column definitions for the summary table. Rendered right of the list. */
  private val SummaryColumns: Vector[Table.Column[(Priority, Int)]] =
    Vector(
      Table.Column[(Priority, Int)]("Priority", width = 8, align = Table.Align.Left, render = _._1.toString),
      Table.Column[(Priority, Int)]("Count", width = 5, align = Table.Align.Right, render = _._2.toString)
    )

  /** Always shows all three priorities with live counts, even when zero. */
  private def summaryRows(tasks: Vector[Task]): Vector[(Priority, Int)] =
    Priorities.map(p => (p, tasks.count(_.priority == p)))

  object App extends TuiApp[Model, Msg]:

    private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Sub.InputKey(ConsoleInputKey.apply, ConsoleInputError.apply, ctx)
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        newTask = TextField.State.withPlaceholder("new task description…"),
        priority = Select.State.of(Priorities, visibleRows = 3).selectIndex(1),
        tasks = ListView.State.of(SeedTasks, visibleRows = 6),
        fm = FocusManager(FocusOrder),
        darkTheme = true
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncTerminalSize(m, ctx)
      msg match
        case NextFocus   => sized.copy(fm = sized.fm.next).tui
        case PrevFocus   => sized.copy(fm = sized.fm.previous).tui
        case ToggleTheme => sized.copy(darkTheme = !sized.darkTheme).tui
        case Quit        => Tui(sized, Cmd.Exit)

        case AddTask =>
          val title = sized.newTask.buffer.trim
          if title.isEmpty then sized.tui
          else
            val priority = sized.priority.value.getOrElse(Priority.Medium)
            val task     = Task(title, priority)
            sized
              .copy(
                tasks = sized.tasks.withItems(sized.tasks.items :+ task),
                newTask = sized.newTask.cleared
              )
              .tui

        case ClearAll =>
          sized.copy(tasks = sized.tasks.withItems(Vector.empty)).tui

        case RemoveSelected =>
          sized.tasks.selectedItem match
            case None => sized.tui
            case Some(task) =>
              val remaining = sized.tasks.items.filterNot(_ == task)
              sized.copy(tasks = sized.tasks.withItems(remaining)).tui

        case ConsoleInputKey(key) =>
          Globals.lookup(key) match
            case Some(next) => update(sized, next, ctx)
            case None       => routeByFocus(sized, key, ctx)

        case ConsoleInputError(_) => sized.tui

    /**
     * Dispatch a key to the focused element. Each element has bespoke
     * routing because their handleKey contracts differ (TextField cares
     * about printable chars; Select has an open/closed state; List
     * consumes arrows internally).
     */
    private def routeByFocus(m: Model, key: KeyDecoder.InputKey, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      import KeyDecoder.InputKey.*
      m.fm.current match
        case Some(id) if id == TaskFieldId =>
          // TextField doesn't consume ↑/↓ — treat them as focus nav.
          ArrowFocusNav.lookup(key) match
            case Some(follow) => update(m, follow, ctx)
            case None =>
              val (next, maybeMsg) = TextField.handleKeyOrSubmit(m.newTask, key)(AddTask)
              val nm               = m.copy(newTask = next)
              maybeMsg.fold(nm.tui)(follow => update(nm, follow, ctx))

        case Some(id) if id == PriorityId =>
          // Select consumes arrows / Enter itself. Delegate everything not
          // already in Globals.
          val (next, _) = Select.handleKey(m.priority, key)(_ => None)
          m.copy(priority = next).tui

        case Some(id) if id == AddId =>
          key match
            case Enter | CharKey(' ') =>
              update(m, AddTask, ctx)
            case _ =>
              ArrowFocusNav
                .lookup(key)
                .orElse(NonTextShortcuts.lookup(key))
                .fold(m.tui)(follow => update(m, follow, ctx))

        case Some(id) if id == ClearId =>
          key match
            case Enter | CharKey(' ') =>
              update(m, ClearAll, ctx)
            case _ =>
              ArrowFocusNav
                .lookup(key)
                .orElse(NonTextShortcuts.lookup(key))
                .fold(m.tui)(follow => update(m, follow, ctx))

        case Some(id) if id == ListId =>
          // List consumes arrows / Home / End / Enter itself. Enter on a
          // row fires onSelect -> RemoveSelected.
          val (next, maybeMsg) = ListView.handleKey(m.tasks, key)(_ => Some(RemoveSelected))
          val nm               = m.copy(tasks = next)
          maybeMsg.fold(nm.tui)(follow => update(nm, follow, ctx))

        case _ =>
          NonTextShortcuts.lookup(key).fold(m.tui)(follow => update(m, follow, ctx))

    override def view(m: Model): RootNode =
      given Theme = if m.darkTheme then Theme.dark else Theme.light
      val theme   = summon[Theme]

      val termWidth  = math.max(80, m.terminalWidth)
      val termHeight = math.max(24, m.terminalHeight)
      val themeName  = if m.darkTheme then "dark" else "light"

      val fieldWidth  = 30
      val selectWidth = 14

      // Row 1: title.
      val title = Layout.Elem(
        TextNode(
          1.x,
          1.y,
          List(Text("Task Catalog", Style(fg = theme.primary, bold = true, underline = true)))
        )
      )

      // Row 3: labels for the input row — widths + gap match `inputRow`
      // so "New:" sits above the text field and "Priority:" above the
      // Select dropdown.
      val labelRow = Layout.row(gap = 4)(
        TextNode(1.x, 1.y, List(Text("New:".padTo(fieldWidth, ' '), Style(fg = theme.foreground)))),
        TextNode(1.x, 1.y, List(Text("Priority:", Style(fg = theme.foreground))))
      )

      // Row 4: text field + Select (dropdown may expand when open).
      val inputRow = Layout.row(gap = 4)(
        TextField.view(m.newTask, lineWidth = fieldWidth, focused = m.fm.isFocused(TaskFieldId)),
        Select.view(m.priority, lineWidth = selectWidth, focused = m.fm.isFocused(PriorityId), render = _.toString)
      )

      // Row: Add / Clear buttons.
      val buttons = Layout.row(gap = 2)(
        Button("Add", focused = m.fm.isFocused(AddId)),
        Button("Clear all", focused = m.fm.isFocused(ClearId))
      )

      // Section header for the tasks list.
      val tasksHeader = Layout.Elem(
        TextNode(
          1.x,
          1.y,
          List(
            Text(s"Tasks (${m.tasks.size})", Style(fg = theme.primary, bold = true, underline = true))
          )
        )
      )

      // Tasks list + summary table side-by-side.
      val listAndTable = Layout.row(gap = 4)(
        ListView.view(
          m.tasks,
          lineWidth = 40,
          focused = m.fm.isFocused(ListId),
          render = (t: Task) => s"[${t.priority}] ${t.title}"
        ),
        Table.view(
          Table.State.of(SummaryColumns, summaryRows(m.tasks.items), visibleRows = 3, selectable = false),
          focused = false
        )
      )

      // Compose into a single column. When the Select is open the column
      // grows, pushing later rows down — the Layout DSL handles that.
      val column = Layout.Column(
        gap = 1,
        children = List(
          title,
          Layout.Spacer(1, 1),
          labelRow,
          inputRow,
          Layout.Spacer(1, 1),
          buttons,
          Layout.Spacer(1, 1),
          tasksHeader,
          listAndTable
        )
      )

      val helpBar = StatusBar(
        left = " catalog ",
        center = "Tab/↑↓ nav  Enter add/select  Ctrl+T theme  Ctrl+C quit",
        right = s" theme=$themeName ",
        width = termWidth,
        at = Coord(1.x, termHeight.y)
      )

      RootNode(
        width = termWidth,
        height = termHeight,
        children = column.resolve(Coord(2.x, 2.y)) :+ helpBar,
        input = None
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.CommandError(input.value))
