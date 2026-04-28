package termflow.apps.showcase

import termflow.apps.dashboard.DashboardApp
import termflow.apps.wizard.WizardApp
import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

import java.nio.file.Path
import java.nio.file.Paths

/**
 * Six-tab showcase of every shipped TermFlow widget and runtime feature.
 *
 * Tabs (switch with `1`..`6` or click the cell in the bar at row 2):
 *
 *   - **1 Showcase** — Stage 1 + Stage 2 demo: capabilities, palette,
 *     themes (RadioGroup), borders (RadioGroup), styles (CheckBox),
 *     unicode width, LogView-driven live input, Tabs widget.
 *   - **2 Widgets** — the Tree widget over a sample file tree (arrow keys
 *     navigate, Space / Enter toggles).
 *   - **3 Inputs** — Form widget composing TextField, Select, Autocomplete,
 *     CheckBox, and Button. Tab/Shift+Tab cycles focus, Enter submits.
 *   - **4 Data** — ListView + Table side-by-side with an animated Spinner
 *     and ProgressBar in the header and a StatusBar pinned at the bottom.
 *   - **5 Layout** — SplitPane horizontal split with a MenuBar in the left
 *     pane; `[`/`]` resize the divider, ←/→/↓ drive the menu.
 *   - **6 Help** — keybinding reference + about.
 *
 * Stage 1+2 features (Showcase tab):
 *
 * **Stage 1**
 *   - **Color depth + capability detection** (PR #157 / issue #148): the
 *     "Capabilities" panel surfaces detected `ColorDepth`, mouse, and
 *     unicode flags from `ctx.terminal.capabilities`; the "Palette" panel
 *     paints a truecolor swatch row that the renderer downgrades on the
 *     fly.
 *   - **Signal-driven resize** (PR #158 / issue #151): resize your
 *     terminal — the middle column reflows live thanks to
 *     `Sub.TerminalResize` wiring SIGWINCH.
 *   - **Themable border characters** (PR #159 / issue #152): `b` cycles
 *     `BorderChars.{sharp, rounded, double, ascii}` (or click the
 *     "Borders" panel rows).
 *   - **Overlay / modal dialog** (PR #160 / issue #153): `d` opens a
 *     confirm modal that suppresses base-view bindings.
 *
 * **Stage 2**
 *   - **Extended SGR attributes** (§5.5): the "Styles" panel renders bold
 *     / italic / dim / underline / reverse / strikethrough / blink. The
 *     terminal's `extendedStyles` capability decides which actually emit.
 *   - **Extended modifier parsing** (§5.6): the "Live input" panel
 *     reflects the most recent decoded key — try Ctrl+ArrowRight,
 *     Shift+F5, Alt+letter.
 *   - **Bracketed paste** (§5.4): paste anything into the terminal — the
 *     "Live input" panel collapses the paste into one event.
 *   - **Unicode width** (§5.3): the "Unicode" panel lays out CJK,
 *     fullwidth ASCII, and emoji over a column ruler.
 *   - **Mouse / SGR-1006** (§5.1): click a row in the **Themes** or
 *     **Borders** panel to switch directly, or hover and scroll the
 *     wheel to cycle. The "Live input" panel shows the decoded
 *     `MouseEvent`.
 *
 * Run with `sbt showcase`.
 */
object Stage1ShowcaseApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  private val borderStyles: Vector[(String, BorderChars)] = Vector(
    "sharp"   -> BorderChars.sharp,
    "rounded" -> BorderChars.rounded,
    "double"  -> BorderChars.double,
    "ascii"   -> BorderChars.ascii
  )

  private val themePresets: Vector[(String, Theme)] = Vector(
    "dark"  -> Theme.dark,
    "light" -> Theme.light,
    "mono"  -> Theme.mono
  )

  // RGB swatch row — the renderer downgrades to the active ColorDepth.
  private val swatchRgb: Vector[Color] = Vector(
    Color.Rgb(255, 64, 64),  // red
    Color.Rgb(255, 160, 32), // orange
    Color.Rgb(255, 224, 64), // yellow
    Color.Rgb(96, 224, 96),  // green
    Color.Rgb(64, 192, 224), // cyan
    Color.Rgb(96, 128, 255), // blue
    Color.Rgb(192, 96, 224)  // purple
  )

  // ---- Layout constants (absolute positioning so mouse hit-testing is exact) ----
  private val topRowY             = 3
  private val topPanelHeight      = 11
  private val bottomRowY          = topRowY + topPanelHeight + 1 // 15
  private val bottomPanelHeight   = 13
  private val capsPanelWidth      = 22
  private val themesPanelWidth    = 22
  private val bordersPanelWidth   = 22
  private val stylesPanelWidth    = 18
  private val liveInputPanelWidth = 36

  /**
   * Bounding box `(col, row, width, height)` of a panel — top-left col/row
   *  are 1-based to match the renderer's coordinate convention.
   */
  final private case class Rect(col: Int, row: Int, width: Int, height: Int):
    def contains(c: Int, r: Int): Boolean =
      c >= col && c < col + width && r >= row && r < row + height

  /** Sample list-select items used by the `l` dialog binding. */
  private val listSelectItems: Vector[String] =
    Vector("apple", "banana", "cherry", "date", "elderberry", "fig", "grape", "honeydew")

  /**
   * How many list rows the `l` dialog renders at once. Used by both the
   *  view (to render) and the mouse hit-test (to map clicks to items).
   */
  private val listSelectMaxVisible: Int = 5

  /**
   * Title shown on the listSelect dialog's top border — declared once so
   *  view / mouse hit-test stay in sync.
   */
  private val listSelectTitle: String = "List select demo"

  /** Max events kept in `Model.eventHistory` for the LogView in the live panel. */
  private val EventHistoryCap: Int = 50

  /**
   * Tab labels — each cell switches the body to a different demo. Order
   * is the source of truth for the digit keyboard shortcuts and for the
   * `activeTab` indices in the model.
   */
  private val showcaseTabs: Vector[String] =
    Vector(
      "1 Showcase",
      "2 Widgets",
      "3 Inputs",
      "4 Data",
      "5 Layout",
      "6 Wizard",
      "7 Dashboard",
      "8 Help"
    )

  /**
   * 0-based row of the Tabs bar inside the root frame. Used by both the
   *  view (to render) and the mouse hit-test (to map clicks).
   */
  private val tabsRow: Int = 2

  /** 0-based column of the first tab cell. Tabs is rendered at (2.x, 2.y). */
  private val tabsCol: Int = 2

  /**
   * Tab index for the file-tree demo so the view function and the
   *  dispatch agree on what `1` selects.
   */
  private val ShowcaseTabIdx: Int  = 0
  private val WidgetsTabIdx: Int   = 1
  private val InputsTabIdx: Int    = 2
  private val DataTabIdx: Int      = 3
  private val LayoutTabIdx: Int    = 4
  private val WizardTabIdx: Int    = 5
  private val DashboardTabIdx: Int = 6
  private val HelpTabIdx: Int      = 7

  // ---- Inputs tab (Form + TextField + Select + Autocomplete + CheckBox + Button) ----
  private val InpNameId: FocusId    = FocusId("inp-name")
  private val InpBioId: FocusId     = FocusId("inp-bio")
  private val InpCountryId: FocusId = FocusId("inp-country")
  private val InpFruitId: FocusId   = FocusId("inp-fruit")
  private val InpAgreeId: FocusId   = FocusId("inp-agree")
  private val InpSubmitId: FocusId  = FocusId("inp-submit")
  private val InputsFocusOrder: Vector[FocusId] =
    Vector(InpNameId, InpBioId, InpCountryId, InpFruitId, InpAgreeId, InpSubmitId)

  private val inputCountries: Vector[String] =
    Vector("Australia", "Brazil", "Canada", "Denmark", "Egypt", "France", "Germany")

  private val inputFruits: Vector[String] = Vector(
    "apple",
    "apricot",
    "banana",
    "blackberry",
    "blueberry",
    "cherry",
    "cranberry",
    "date",
    "elderberry",
    "fig",
    "grape",
    "grapefruit",
    "honeydew",
    "kiwi",
    "lemon",
    "lime",
    "mango",
    "melon",
    "nectarine",
    "orange",
    "papaya",
    "peach",
    "pear",
    "pineapple",
    "plum",
    "raspberry",
    "strawberry",
    "tangerine",
    "watermelon"
  )

  /** True when focus on the Inputs tab is on a key-consuming text widget. */
  private def inputsTextFocused(m: Model): Boolean =
    m.activeTab == InputsTabIdx && {
      val cur = m.inputsFocus.current
      cur.contains(InpNameId) || cur.contains(InpBioId) || cur.contains(InpFruitId) ||
      (cur.contains(InpCountryId) && m.inputsCountry.open)
    }

  // ---- Data tab (ListView + Table + Spinner + ProgressBar + StatusBar) ----
  private val DataListId: FocusId             = FocusId("data-list")
  private val DataTableId: FocusId            = FocusId("data-table")
  private val DataFocusOrder: Vector[FocusId] = Vector(DataListId, DataTableId)

  /** Sample task names for the ListView/Table demo on the Data tab. */
  private val dataTasks: Vector[String] = Vector(
    "Buy groceries",
    "Write report",
    "Call alice",
    "Review PR",
    "Read book",
    "Refactor module",
    "Fix bug #123",
    "Plan vacation",
    "Update docs",
    "Send email",
    "Renew passport",
    "Schedule meeting"
  )

  /** Stable status row per task — shown in the Table on the Data tab. */
  private val dataStatuses: Vector[String] = Vector(
    "todo",
    "doing",
    "done",
    "todo",
    "todo",
    "doing",
    "blocked",
    "todo",
    "doing",
    "done",
    "todo",
    "doing"
  )

  /** Columns for the Data tab's Table widget. */
  private val DataColumns: Vector[widgets.Table.Column[(String, String)]] = Vector(
    widgets.Table.Column[(String, String)](
      "Task",
      width = 22,
      align = widgets.Table.Align.Left,
      render = _._1
    ),
    widgets.Table.Column[(String, String)](
      "Status",
      width = 8,
      align = widgets.Table.Align.Left,
      render = _._2
    )
  )

  // ---- Layout tab (SplitPane + MenuBar) ----
  private val layoutMenus: Vector[widgets.MenuBar.Menu] = Vector(
    widgets.MenuBar.Menu("File", Vector("New", "Open…", "Save", "Save As…", "Quit")),
    widgets.MenuBar.Menu("Edit", Vector("Cut", "Copy", "Paste", "Find…")),
    widgets.MenuBar.Menu("View", Vector("Zoom in", "Zoom out", "Reset"))
  )

  /**
   * Sample file-tree used by the Widgets tab to demo the Tree widget.
   * Plain ADT — the widget's `Children` instance traverses it.
   */
  sealed trait DemoNode:
    def name: String
  final case class DemoDir(name: String, kids: Vector[DemoNode]) extends DemoNode
  final case class DemoFile(name: String)                        extends DemoNode

  given widgets.Tree.Children[DemoNode, String] with
    def id(n: DemoNode): String = n.name
    def kids(n: DemoNode): Vector[DemoNode] = n match
      case DemoDir(_, k) => k
      case _: DemoFile   => Vector.empty

  private val demoTree: Vector[DemoNode] = Vector(
    DemoDir(
      "termflow",
      Vector(
        DemoDir(
          "tui",
          Vector(
            DemoFile("AnsiRenderer.scala"),
            DemoFile("Layout.scala"),
            DemoFile("Theme.scala"),
            DemoDir("widgets", Vector(DemoFile("Tabs.scala"), DemoFile("LogView.scala"), DemoFile("Tree.scala")))
          )
        ),
        DemoFile("README.md")
      )
    ),
    DemoFile("build.sbt")
  )

  /** Whether a [[Dialog.FilePicker]] is showing files + directories or only directories. */
  enum FilePickerMode:
    case Files
    case Directories

  enum Dialog:
    case None
    case ConfirmQuit(yesFocused: Boolean)
    case TextInput(state: Prompt.State, okFocused: Boolean)
    case ListSelect(selectedIndex: Int)
    case Waiting(openedAtTick: Long, autoCloseAtTick: Long)
    case FilePicker(
      mode: FilePickerMode,
      currentPath: Path,
      entries: Vector[Dialogs.FileEntry],
      selectedIndex: Int
    )

  final case class Model(
    width: Int,
    height: Int,
    borderIdx: Int,
    themeIdx: Int,
    dialog: Dialog,
    capabilities: Capabilities,
    termName: String,
    /** Most recent input event, formatted for display in the live panel. */
    lastEvent: Option[String],
    /**
     * Bounded history of recent events fed to the live-input panel via the
     * LogView widget. Latest events live at the tail.
     */
    eventHistory: Vector[String],
    /** Last value submitted from a textInput dialog (rendered in Live input). */
    lastTextInput: Option[String],
    /** Last item picked from a listSelect dialog. */
    lastListPick: Option[String],
    /** Last value picked from a fileDialog / directoryDialog. */
    lastFilePick: Option[String],
    /** Tick counter incremented by `tickSub`; drives the waiting spinner. */
    tick: Long,
    /** 0-based index of the currently selected tab. See `showcaseTabs`. */
    activeTab: Int,
    /** Expanded ids in the Widgets tab's Tree. */
    treeExpanded: Set[String],
    /** Highlighted row index in the Widgets tab's Tree. */
    treeSelected: Int,
    // ---- Inputs tab state ----
    inputsFocus: FocusManager,
    inputsName: widgets.TextField.State,
    inputsBio: widgets.TextField.State,
    inputsCountry: widgets.Select.State[String],
    inputsFruit: widgets.Autocomplete.State[String],
    inputsAgree: Boolean,
    inputsSubmitted: Option[String],
    // ---- Data tab state ----
    dataFocus: FocusManager,
    dataList: widgets.ListView.State[String],
    dataProgress: Double,
    // ---- Layout tab state ----
    layoutSplitRatio: Double,
    layoutMenu: widgets.MenuBar.State,
    layoutLastPick: Option[String],
    // ---- Embedded sub-apps ----
    wizardModel: WizardApp.Model,
    dashboardModel: DashboardApp.Model,
    input: Sub[Msg],
    resize: Sub[Msg],
    tickSub: Sub[Msg]
  ):
    def borderName: String = borderStyles(borderIdx)._1
    def chars: BorderChars = borderStyles(borderIdx)._2
    def themeName: String  = themePresets(themeIdx)._1
    def baseTheme: Theme   = themePresets(themeIdx)._2

    /** Theme with the user-selected border chars folded in. */
    def theme: Theme = baseTheme.copy(chars = chars)

  enum Msg:
    case Resize(w: Int, h: Int)
    case Tick
    case CycleBorder
    case CycleTheme
    case SelectThemeIdx(i: Int)
    case SelectBorderIdx(i: Int)
    case SelectListIdx(i: Int)
    case SelectTab(idx: Int)
    case ToggleTreeNode
    case TreeUp
    case TreeDown
    case OpenDialog
    case OpenTextInput
    case OpenListSelect
    case OpenWaiting
    case OpenFileDialog
    case OpenDirectoryDialog
    case FilePickerMove(delta: Int)
    case FilePickerActivate
    case FilePickerAccept(path: String)
    case CloseDialog
    case ToggleDialogFocus
    case TextInputAccept(value: String)
    case ListSelectAccept(index: Int)
    case InputsSubmit
    case LayoutMenuPick(menuTitle: String, item: String)
    case Quit
    case Key(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:

    /** Spinner runs at 10 fps; cheap, makes the waiting overlay actually animate. */
    private val tickPeriodMs = 100L

    /** Auto-close the waiting dialog 30 ticks (~3s) after it opens. */
    private val waitingDurationTicks = 30L

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        width = ctx.terminal.width,
        height = ctx.terminal.height,
        borderIdx = 1, // start on rounded — visibly different from default
        themeIdx = 0,
        dialog = Dialog.None,
        capabilities = ctx.terminal.capabilities,
        termName = sys.env.getOrElse("TERM", "?"),
        lastEvent = None,
        eventHistory = Vector.empty,
        lastTextInput = None,
        lastListPick = None,
        lastFilePick = None,
        tick = 0L,
        activeTab = ShowcaseTabIdx,
        treeExpanded = Set("termflow", "tui"),
        treeSelected = 0,
        inputsFocus = FocusManager(InputsFocusOrder),
        inputsName = widgets.TextField.State.withPlaceholder("Alice"),
        inputsBio = widgets.TextField.State.withPlaceholder("(short bio)"),
        inputsCountry = widgets.Select.State.of(inputCountries, visibleRows = 4).selectIndex(2),
        inputsFruit = widgets.Autocomplete.State.of(inputFruits),
        inputsAgree = false,
        inputsSubmitted = None,
        dataFocus = FocusManager(DataFocusOrder),
        dataList = widgets.ListView.State.of(dataTasks, visibleRows = 8),
        dataProgress = 0.0,
        layoutSplitRatio = 0.55,
        layoutMenu = widgets.MenuBar.State(layoutMenus),
        layoutLastPick = None,
        wizardModel = WizardApp.initialModel,
        dashboardModel = DashboardApp.initialModel,
        input = Sub.InputKey(k => Key(k), e => KeyError(e), ctx),
        resize = Sub.TerminalResize[Msg](200, (w, h) => Resize(w, h), ctx),
        tickSub = Sub.Every(tickPeriodMs, () => Tick, ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Resize(w, h)       => m.copy(width = w, height = h).tui
        case Tick               => onTick(m)
        case CycleBorder        => m.copy(borderIdx = (m.borderIdx + 1) % borderStyles.size).tui
        case CycleTheme         => m.copy(themeIdx = (m.themeIdx + 1) % themePresets.size).tui
        case SelectThemeIdx(i)  => m.copy(themeIdx = clampIdx(i, themePresets.size)).tui
        case SelectBorderIdx(i) => m.copy(borderIdx = clampIdx(i, borderStyles.size)).tui
        case OpenDialog         => m.copy(dialog = Dialog.ConfirmQuit(yesFocused = false)).tui
        case OpenTextInput =>
          m.copy(dialog = Dialog.TextInput(state = Prompt.State(), okFocused = true)).tui
        case OpenListSelect =>
          m.copy(dialog = Dialog.ListSelect(selectedIndex = 0)).tui
        case OpenWaiting =>
          m.copy(dialog = Dialog.Waiting(openedAtTick = m.tick, autoCloseAtTick = m.tick + waitingDurationTicks)).tui
        case OpenFileDialog      => openFilePicker(m, FilePickerMode.Files)
        case OpenDirectoryDialog => openFilePicker(m, FilePickerMode.Directories)
        case FilePickerMove(delta) =>
          m.dialog match
            case Dialog.FilePicker(mode, p, entries, idx) if entries.nonEmpty =>
              val next = math.max(0, math.min(entries.size - 1, idx + delta))
              m.copy(dialog = Dialog.FilePicker(mode, p, entries, next)).tui
            case _ => m.tui
        case FilePickerActivate =>
          m.dialog match
            case Dialog.FilePicker(mode, p, entries, idx) if entries.nonEmpty =>
              entries(idx) match
                case Dialogs.FileEntry.Parent =>
                  refreshFilePicker(m, mode, Option(p.getParent).getOrElse(p))
                case Dialogs.FileEntry.Directory(name) =>
                  refreshFilePicker(m, mode, p.resolve(name))
                case Dialogs.FileEntry.File(name, _) =>
                  mode match
                    case FilePickerMode.Files =>
                      val picked = p.resolve(name).toString
                      m.copy(dialog = Dialog.None, lastFilePick = Some(picked)).tui
                    case FilePickerMode.Directories => m.tui
            case _ => m.tui
        case FilePickerAccept(path) =>
          m.copy(dialog = Dialog.None, lastFilePick = Some(path)).tui
        case CloseDialog => m.copy(dialog = Dialog.None).tui
        case ToggleDialogFocus =>
          m.dialog match
            case Dialog.ConfirmQuit(yes)        => m.copy(dialog = Dialog.ConfirmQuit(!yes)).tui
            case Dialog.TextInput(state, focus) => m.copy(dialog = Dialog.TextInput(state, !focus)).tui
            case _                              => m.tui
        case TextInputAccept(value) =>
          m.copy(lastTextInput = Some(value), dialog = Dialog.None).tui
        case ListSelectAccept(idx) =>
          m.copy(lastListPick = Some(listSelectItems(clampIdx(idx, listSelectItems.size))), dialog = Dialog.None).tui
        case InputsSubmit =>
          val name    = if m.inputsName.buffer.nonEmpty then m.inputsName.buffer else "<unnamed>"
          val country = m.inputsCountry.value.getOrElse("?")
          val fruit   = m.inputsFruit.selected.getOrElse("?")
          val agree   = if m.inputsAgree then "agreed" else "not agreed"
          m.copy(inputsSubmitted = Some(s"$name • $country • $fruit • $agree")).tui
        case LayoutMenuPick(menuTitle, item) =>
          m.copy(layoutLastPick = Some(s"$menuTitle › $item")).tui
        case SelectListIdx(i) =>
          m.dialog match
            case Dialog.ListSelect(_) =>
              m.copy(dialog = Dialog.ListSelect(clampIdx(i, listSelectItems.size))).tui
            case _ => m.tui
        case SelectTab(idx) =>
          m.copy(activeTab = clampIdx(idx, showcaseTabs.size)).tui
        case ToggleTreeNode =>
          val rows = widgets.Tree.visibleRows(demoTree, m.treeExpanded)
          if rows.isEmpty then m.tui
          else
            val row = rows(clampIdx(m.treeSelected, rows.size))
            if !row.hasChildren then m.tui
            else
              val next =
                if row.expanded then m.treeExpanded - row.id
                else m.treeExpanded + row.id
              m.copy(treeExpanded = next).tui
        case TreeUp =>
          m.copy(treeSelected = math.max(0, m.treeSelected - 1)).tui
        case TreeDown =>
          val rows = widgets.Tree.visibleRows(demoTree, m.treeExpanded)
          m.copy(treeSelected = math.min(math.max(0, rows.size - 1), m.treeSelected + 1)).tui
        case Quit        => Tui(m, Cmd.Exit)
        case KeyError(_) => m.tui
        case Key(k)      =>
          // Dialog.TextInput edits its prompt state through Prompt.handleKey
          // before dispatch. Other dialogs / the base view route through dispatch.
          val formatted = formatEvent(k)
          val withEvent = m.copy(
            lastEvent = Some(formatted),
            eventHistory = (m.eventHistory :+ formatted).takeRight(EventHistoryCap)
          )
          withEvent.dialog match
            case Dialog.TextInput(state, focus) =>
              val (nextState, maybeCmd) =
                Prompt.handleKey[Msg](state, k)(line => Right(TextInputAccept(line.value)))
              val nextDialog = Dialog.TextInput(nextState, focus)
              val nextModel  = withEvent.copy(dialog = nextDialog)
              maybeCmd match
                case Some(cmd) => Tui(nextModel, cmd)
                case None      => Tui(nextModel, dispatch(nextModel, k))
            case Dialog.None =>
              // Tabs with stateful widgets (Inputs, Data, Layout) handle
              // keys directly so they can fold the keystroke into widget
              // state. Other tabs fall through to the Cmd-based dispatch.
              withEvent.activeTab match
                case InputsTabIdx    => handleInputsKey(withEvent, k, ctx)
                case DataTabIdx      => handleDataKey(withEvent, k, ctx)
                case LayoutTabIdx    => handleLayoutKey(withEvent, k, ctx)
                case WizardTabIdx    => handleWizardKey(withEvent, k)
                case DashboardTabIdx => handleDashboardKey(withEvent, k)
                case _               => Tui(withEvent, dispatch(withEvent, k))
            case _ => Tui(withEvent, dispatch(withEvent, k))

    /**
     * Tick handler: advance the counter, animate the Data tab's progress
     *  bar, auto-close Waiting when its duration elapses, drive the
     *  embedded dashboard simulation when its tab is active, and
     *  otherwise leave the model alone.
     */
    private def onTick(m: Model): Tui[Model, Msg] =
      val nextProgress =
        if m.dataProgress >= 1.0 then 0.0
        else math.min(1.0, m.dataProgress + 0.01)
      val nextDashboard =
        if m.activeTab == DashboardTabIdx then DashboardApp.step(m.dashboardModel, DashboardApp.Msg.Tick)
        else m.dashboardModel
      val tickedModel = m.copy(tick = m.tick + 1, dataProgress = nextProgress, dashboardModel = nextDashboard)
      tickedModel.dialog match
        case Dialog.Waiting(_, deadline) if tickedModel.tick >= deadline =>
          tickedModel.copy(dialog = Dialog.None).tui
        case _ => tickedModel.tui

    /**
     * Forward a keystroke to the embedded wizard. `q`/`Esc` outside a
     * `TextField` opens the showcase quit dialog instead of quitting the
     * wizard's standalone runtime — embedding semantics differ from the
     * standalone app on quit only.
     */
    private def handleWizardKey(m: Model, k: KeyDecoder.InputKey): Tui[Model, Msg] =
      import KeyDecoder.InputKey.*
      // Tab-switch digits and tab-bar mouse hits stay routed through the
      // showcase, not the embedded wizard.
      digitTabSwitch(k, allowDigits = !m.wizardModel.isFocusedTextField) match
        case Some(idx) => Tui(m, Cmd.GCmd(SelectTab(idx)))
        case None =>
          k match
            case Mouse(ev) => Tui(m, tabBarMouseDispatch(ev))
            case CharKey('q') | CharKey('Q') | Escape if !m.wizardModel.isFocusedTextField =>
              Tui(m, Cmd.GCmd(OpenDialog))
            case _ =>
              val nextWizard = WizardApp.step(m.wizardModel, WizardApp.Msg.Key(k))
              m.copy(wizardModel = nextWizard).tui

    /**
     * Forward a keystroke to the embedded dashboard. Dashboard never has
     * a TextField focused, so quit semantics are simpler than the wizard.
     */
    private def handleDashboardKey(m: Model, k: KeyDecoder.InputKey): Tui[Model, Msg] =
      import KeyDecoder.InputKey.*
      digitTabSwitch(k, allowDigits = true) match
        case Some(idx) => Tui(m, Cmd.GCmd(SelectTab(idx)))
        case None =>
          k match
            case Mouse(ev)                            => Tui(m, tabBarMouseDispatch(ev))
            case CharKey('q') | CharKey('Q') | Escape => Tui(m, Cmd.GCmd(OpenDialog))
            case _ =>
              val nextDashboard = DashboardApp.step(m.dashboardModel, DashboardApp.Msg.Key(k))
              m.copy(dashboardModel = nextDashboard).tui

    private def clampIdx(i: Int, size: Int): Int = math.max(0, math.min(size - 1, i))

    /** Start path for the file pickers — `user.home`, defaulting to `.` if unset. */
    private val filePickerStartPath: Path = Paths.get(sys.props.getOrElse("user.home", "."))

    private def openFilePicker(m: Model, mode: FilePickerMode): Tui[Model, Msg] =
      refreshFilePicker(m, mode, filePickerStartPath)

    /**
     * (Re)list `path` and replace the FilePicker dialog state. On listing
     * failure the dialog is closed and the error message is shown via
     * `lastFilePick`.
     */
    private def refreshFilePicker(m: Model, mode: FilePickerMode, path: Path): Tui[Model, Msg] =
      Dialogs.listEntries(path) match
        case Right(entries) =>
          val filtered = mode match
            case FilePickerMode.Files       => entries
            case FilePickerMode.Directories => entries.filter(_.isDirectory)
          m.copy(dialog = Dialog.FilePicker(mode, path, filtered, 0)).tui
        case Left(err) =>
          m.copy(dialog = Dialog.None, lastFilePick = Some(s"error: $err")).tui

    private def dispatch(m: Model, k: KeyDecoder.InputKey): Cmd[Msg] =
      import KeyDecoder.InputKey.*
      m.dialog match
        case Dialog.ConfirmQuit(yesFocused) =>
          k match
            case ArrowLeft | ArrowRight => Cmd.GCmd(ToggleDialogFocus)
            case Enter                  => if yesFocused then Cmd.GCmd(Quit) else Cmd.GCmd(CloseDialog)
            case Escape                 => Cmd.GCmd(CloseDialog)
            case Mouse(_)               => Cmd.NoCmd
            case _                      => Cmd.NoCmd

        case Dialog.TextInput(_, _) =>
          k match
            // Esc cancels; Tab moves focus between OK and Cancel.
            case Escape => Cmd.GCmd(CloseDialog)
            case CharKey('\t') | KeyDecoder.InputKey.BackTab =>
              Cmd.GCmd(ToggleDialogFocus)
            case _ => Cmd.NoCmd

        case Dialog.ListSelect(idx) =>
          k match
            case ArrowUp =>
              val next = math.max(0, idx - 1)
              Cmd.GCmd(SelectListIdx(next))
            case ArrowDown =>
              val next = math.min(listSelectItems.size - 1, idx + 1)
              Cmd.GCmd(SelectListIdx(next))
            case Home      => Cmd.GCmd(SelectListIdx(0))
            case End       => Cmd.GCmd(SelectListIdx(listSelectItems.size - 1))
            case Enter     => Cmd.GCmd(ListSelectAccept(idx))
            case Escape    => Cmd.GCmd(CloseDialog)
            case Mouse(ev) => listSelectMouseDispatch(m, idx, ev)
            case _         => Cmd.NoCmd

        case Dialog.Waiting(_, _) =>
          k match
            case Escape => Cmd.GCmd(CloseDialog)
            case _      => Cmd.NoCmd

        case Dialog.FilePicker(_, _, _, _) =>
          k match
            case ArrowUp   => Cmd.GCmd(FilePickerMove(-1))
            case ArrowDown => Cmd.GCmd(FilePickerMove(+1))
            case PageUp    => Cmd.GCmd(FilePickerMove(-5))
            case PageDown  => Cmd.GCmd(FilePickerMove(+5))
            case Enter     => Cmd.GCmd(FilePickerActivate)
            case Escape    => Cmd.GCmd(CloseDialog)
            case _         => Cmd.NoCmd

        case Dialog.None =>
          // Tab-switch shortcuts work on every tab.
          digitTabSwitch(k, allowDigits = true) match
            case Some(idx) => Cmd.GCmd(SelectTab(idx))
            case None      =>
              // Per-tab key bindings.
              m.activeTab match
                case WidgetsTabIdx => widgetsTabKey(k, m)
                case HelpTabIdx    => helpTabKey(k)
                case _             => showcaseTabKey(k, m)

    /** Key bindings for the main "Showcase" tab — the original Stage 1+2 demo. */
    private def showcaseTabKey(k: KeyDecoder.InputKey, m: Model): Cmd[Msg] =
      import KeyDecoder.InputKey.*
      k match
        case CharKey('b') | CharKey('B') => Cmd.GCmd(CycleBorder)
        case CharKey('t') | CharKey('T') => Cmd.GCmd(CycleTheme)
        case CharKey('d') | CharKey('D') => Cmd.GCmd(OpenDialog)
        case CharKey('i') | CharKey('I') => Cmd.GCmd(OpenTextInput)
        case CharKey('l') | CharKey('L') => Cmd.GCmd(OpenListSelect)
        case CharKey('w') | CharKey('W') => Cmd.GCmd(OpenWaiting)
        case CharKey('f') | CharKey('F') => Cmd.GCmd(OpenFileDialog)
        case CharKey('g') | CharKey('G') => Cmd.GCmd(OpenDirectoryDialog)
        case CharKey('q') | CharKey('Q') => Cmd.GCmd(OpenDialog)
        case Escape                      => Cmd.GCmd(OpenDialog)
        case Mouse(ev)                   => mouseDispatch(m, ev)
        case _                           => Cmd.NoCmd

    /** Key bindings for the Widgets tab — tree navigation. */
    private def widgetsTabKey(k: KeyDecoder.InputKey, m: Model): Cmd[Msg] =
      import KeyDecoder.InputKey.*
      k match
        case ArrowUp                     => Cmd.GCmd(TreeUp)
        case ArrowDown                   => Cmd.GCmd(TreeDown)
        case Enter | CharKey(' ')        => Cmd.GCmd(ToggleTreeNode)
        case CharKey('q') | CharKey('Q') => Cmd.GCmd(OpenDialog)
        case Escape                      => Cmd.GCmd(OpenDialog)
        case Mouse(ev)                   => tabBarMouseDispatch(ev)
        case _                           => Cmd.NoCmd

    /** Key bindings for the Help tab. */
    private def helpTabKey(k: KeyDecoder.InputKey): Cmd[Msg] =
      import KeyDecoder.InputKey.*
      k match
        case CharKey('q') | CharKey('Q') => Cmd.GCmd(OpenDialog)
        case Escape                      => Cmd.GCmd(OpenDialog)
        // Route mouse so clicks on the Tabs bar still switch tabs from
        // here — without this, the Help tab traps the user.
        case Mouse(ev) => tabBarMouseDispatch(ev)
        case _         => Cmd.NoCmd

    /**
     * Map a digit-key tab switch to the matching tab index, gated on
     * whether a text-consuming widget currently has focus (in which case
     * the digit should be inserted instead of switching tabs).
     */
    private def digitTabSwitch(k: KeyDecoder.InputKey, allowDigits: Boolean): Option[Int] =
      import KeyDecoder.InputKey.*
      if !allowDigits then None
      else
        k match
          case CharKey(c) if c >= '1' && c <= ('0' + showcaseTabs.size).toChar =>
            Some(c - '1')
          case _ => None

    /**
     * Key bindings for the Inputs tab — TextField / Select / Autocomplete
     * / CheckBox / Button composed via the [[Form]] widget. Updates the
     * model directly so widget state can be folded in without a round
     * trip through dispatch.
     */
    private def handleInputsKey(m: Model, k: KeyDecoder.InputKey, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      import KeyDecoder.InputKey.*
      val textFocused = inputsTextFocused(m)

      // Globals first — Tab/Shift+Tab move focus, Esc opens quit dialog,
      // then digit-tab-switch (always-on, like the other showcase tabs),
      // then route to the focused widget. Letter `q` quit is gated on
      // text focus so typing letters into a TextField stays unaffected;
      // digits collide with showcase tab shortcuts and are reserved.
      val pre: Option[Tui[Model, Msg]] = k match
        case CharKey('\t')                               => Some(m.copy(inputsFocus = m.inputsFocus.next).tui)
        case BackTab                                     => Some(m.copy(inputsFocus = m.inputsFocus.previous).tui)
        case Escape                                      => Some(Tui(m, Cmd.GCmd(OpenDialog)))
        case CharKey('q') | CharKey('Q') if !textFocused => Some(Tui(m, Cmd.GCmd(OpenDialog)))
        case Mouse(ev)                                   => Some(Tui(m, tabBarMouseDispatch(ev)))
        case _ =>
          digitTabSwitch(k, allowDigits = true).map(idx => Tui(m, Cmd.GCmd(SelectTab(idx))))

      pre.getOrElse {
        // Route the remaining key into the focused widget.
        m.inputsFocus.current match
          case Some(id) if id == InpNameId =>
            val (next, maybe) = widgets.TextField.handleKeyOrSubmit(m.inputsName, k)(InputsSubmit)
            val nm            = m.copy(inputsName = next)
            maybe.fold(nm.tui)(msg => update(nm, msg, ctx))

          case Some(id) if id == InpBioId =>
            val (next, maybe) = widgets.TextField.handleKeyOrSubmit(m.inputsBio, k)(InputsSubmit)
            val nm            = m.copy(inputsBio = next)
            maybe.fold(nm.tui)(msg => update(nm, msg, ctx))

          case Some(id) if id == InpCountryId =>
            val (next, _) = widgets.Select.handleKey(m.inputsCountry, k)((_: String) => None)
            m.copy(inputsCountry = next).tui

          case Some(id) if id == InpFruitId =>
            val res = widgets.Autocomplete.handleKey(m.inputsFruit, k)
            val nm  = m.copy(inputsFruit = res.state)
            if res.picked.isDefined then update(nm, InputsSubmit, ctx) else nm.tui

          case Some(id) if id == InpAgreeId =>
            k match
              case Enter | CharKey(' ') => m.copy(inputsAgree = !m.inputsAgree).tui
              case ArrowDown            => m.copy(inputsFocus = m.inputsFocus.next).tui
              case ArrowUp              => m.copy(inputsFocus = m.inputsFocus.previous).tui
              case _                    => m.tui

          case Some(id) if id == InpSubmitId =>
            k match
              case Enter | CharKey(' ') => update(m, InputsSubmit, ctx)
              case ArrowDown            => m.copy(inputsFocus = m.inputsFocus.next).tui
              case ArrowUp              => m.copy(inputsFocus = m.inputsFocus.previous).tui
              case _                    => m.tui

          case _ => m.tui
      }

    /**
     * Key bindings for the Data tab — ListView + Table side-by-side.
     * Tab cycles focus between the two; arrow keys navigate the focused
     * widget.
     */
    private def handleDataKey(m: Model, k: KeyDecoder.InputKey, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val _ = ctx
      import KeyDecoder.InputKey.*
      val pre: Option[Tui[Model, Msg]] = k match
        case CharKey('\t')               => Some(m.copy(dataFocus = m.dataFocus.next).tui)
        case BackTab                     => Some(m.copy(dataFocus = m.dataFocus.previous).tui)
        case Escape                      => Some(Tui(m, Cmd.GCmd(OpenDialog)))
        case CharKey('q') | CharKey('Q') => Some(Tui(m, Cmd.GCmd(OpenDialog)))
        case Mouse(ev)                   => Some(Tui(m, tabBarMouseDispatch(ev)))
        case _ => digitTabSwitch(k, allowDigits = true).map(idx => Tui(m, Cmd.GCmd(SelectTab(idx))))

      pre.getOrElse {
        // ListView consumes arrows itself; the Table on the right tracks
        // the same selection so its cursor moves with the list cursor.
        m.dataFocus.current match
          case Some(_) =>
            val (next, _) = widgets.ListView.handleKey[String, Msg](m.dataList, k)(_ => None)
            m.copy(dataList = next).tui
          case None => m.tui
      }

    /**
     * Key bindings for the Layout tab — SplitPane + MenuBar. The MenuBar
     * widget owns navigation when it's open; otherwise `[` / `]` adjust
     * the split ratio so the divider can move.
     */
    private def handleLayoutKey(m: Model, k: KeyDecoder.InputKey, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      import KeyDecoder.InputKey.*
      val menuActive = m.layoutMenu.openIdx.isDefined
      val pre: Option[Tui[Model, Msg]] = k match
        case CharKey('q') | CharKey('Q') => Some(Tui(m, Cmd.GCmd(OpenDialog)))
        case Escape if !menuActive       => Some(Tui(m, Cmd.GCmd(OpenDialog)))
        case Mouse(ev)                   => Some(Tui(m, tabBarMouseDispatch(ev)))
        case CharKey('[') =>
          Some(m.copy(layoutSplitRatio = math.max(widgets.SplitPane.MinSizeRatio, m.layoutSplitRatio - 0.05)).tui)
        case CharKey(']') =>
          Some(m.copy(layoutSplitRatio = math.min(1.0 - widgets.SplitPane.MinSizeRatio, m.layoutSplitRatio + 0.05)).tui)
        case _ =>
          digitTabSwitch(k, allowDigits = !menuActive).map(idx => Tui(m, Cmd.GCmd(SelectTab(idx))))

      pre.getOrElse {
        // Everything else drives the MenuBar.
        val res = widgets.MenuBar.handleKey(m.layoutMenu, k)
        val nm  = m.copy(layoutMenu = res.state)
        res.picked match
          case Some((mi, ii)) =>
            val title = layoutMenus(mi).title
            val item  = layoutMenus(mi).items(ii)
            update(nm, LayoutMenuPick(title, item), ctx)
          case None => nm.tui
      }

    /**
     * Mouse handling reduced to just the tabs bar — for the non-Showcase
     *  tabs that don't have any mouse-targetable panels of their own.
     */
    private def tabBarMouseDispatch(ev: MouseEvent): Cmd[Msg] =
      ev match
        case MouseEvent.Press(MouseButton.Left, col, row, _) =>
          tabHitTest(col, row).map(idx => Cmd.GCmd[Msg](SelectTab(idx))).getOrElse(Cmd.NoCmd)
        case _ => Cmd.NoCmd

    /**
     * Map a [[MouseEvent]] to a model command using static panel rects.
     *
     *   - Press inside the Themes panel's row strip → [[SelectThemeIdx]].
     *   - Press inside the Borders panel's row strip → [[SelectBorderIdx]].
     *   - Scroll wheel inside either panel cycles through entries.
     *
     * Returns [[Cmd.NoCmd]] for releases, drags, and clicks outside any
     * known target — the live-input panel still reflects the raw event.
     */
    private def mouseDispatch(m: Model, ev: MouseEvent): Cmd[Msg] =
      val (col, row) = ev.at
      val themesR    = themesRect(m)
      val bordersR   = bordersRect(m)

      ev match
        case MouseEvent.Press(MouseButton.Left, _, _, _) =>
          // Tab bar takes priority — clicks on a tab cell switch tabs
          // regardless of which content tab is currently shown.
          tabHitTest(col, row) match
            case Some(idx) => Cmd.GCmd(SelectTab(idx))
            case None =>
              if themesR.contains(col, row) then
                themeIndexAtRow(themesR, row).map(i => Cmd.GCmd[Msg](SelectThemeIdx(i))).getOrElse(Cmd.NoCmd)
              else if bordersR.contains(col, row) then
                borderIndexAtRow(bordersR, row).map(i => Cmd.GCmd[Msg](SelectBorderIdx(i))).getOrElse(Cmd.NoCmd)
              else Cmd.NoCmd

        case MouseEvent.Scroll(dir, _, _, _) =>
          if themesR.contains(col, row) then
            val next = nextIndex(m.themeIdx, themePresets.size, dir)
            Cmd.GCmd(SelectThemeIdx(next))
          else if bordersR.contains(col, row) then
            val next = nextIndex(m.borderIdx, borderStyles.size, dir)
            Cmd.GCmd(SelectBorderIdx(next))
          else Cmd.NoCmd

        case _ => Cmd.NoCmd

    private def nextIndex(current: Int, size: Int, dir: ScrollDirection): Int =
      dir match
        case ScrollDirection.Up | ScrollDirection.Left =>
          (current - 1 + size) % size
        case ScrollDirection.Down | ScrollDirection.Right =>
          (current + 1) % size

    /**
     * Construct the listSelect Overlay used both for rendering and for
     *  mouse hit-testing. Centralising the call keeps the title /
     *  maxVisible / theme parameters consistent.
     */
    private def buildListSelectOverlay(idx: Int)(using Theme): Overlay =
      Dialogs.listSelect(
        title = listSelectTitle,
        items = listSelectItems,
        selectedIndex = idx,
        maxVisible = listSelectMaxVisible
      )

    /**
     * Resolved on-screen rectangle of the listSelect dialog at the
     *  current selection. Mouse hit-tests use this to map clicks to
     *  rows.
     */
    private def listSelectRect(m: Model, idx: Int): Rect =
      given Theme  = m.theme
      val o        = buildListSelectOverlay(idx)
      val (ox, oy) = OverlayPosition.resolve(o.position, o.width, o.height, m.width, m.height)
      Rect(col = ox.value, row = oy.value, width = o.width, height = o.height)

    /**
     * Mouse handling while the listSelect dialog is open: clicks on a
     *  visible row commit that item, scroll-wheel cycles the selection.
     */
    private def listSelectMouseDispatch(m: Model, currentIdx: Int, ev: MouseEvent): Cmd[Msg] =
      val (col, row) = ev.at
      val rect       = listSelectRect(m, currentIdx)
      val layout     = Dialogs.listSelectLayout(listSelectItems.size, currentIdx, listSelectMaxVisible)

      if !rect.contains(col, row) then Cmd.NoCmd
      else
        ev match
          case MouseEvent.Press(MouseButton.Left, _, _, _) =>
            val firstItemRowAbs = rect.row + (layout.firstRowOffset - 1)
            val itemOffset      = row - firstItemRowAbs
            if itemOffset >= 0 && itemOffset < layout.visibleCount then
              val itemIdx = layout.anchorIndex + itemOffset
              if itemIdx < listSelectItems.size then Cmd.GCmd(ListSelectAccept(itemIdx))
              else Cmd.NoCmd
            else Cmd.NoCmd
          case MouseEvent.Scroll(dir, _, _, _) =>
            val next = nextIndex(currentIdx, listSelectItems.size, dir)
            Cmd.GCmd(SelectListIdx(next))
          case _ => Cmd.NoCmd

    /**
     * Inside a panel rendered via [[panel]], the BoxNode draws its border on
     * the panel's first row and `translateInto` shifts every child down by
     * `panel.row - 1`. A row authored at panel-local Y=3 (the first list
     * row in `themesPanel` / `bordersPanel`) therefore lands at absolute
     * row `3 + (panel.row - 1) = panel.row + 2`. Each subsequent item is
     * one row below.
     */
    private def themeIndexAtRow(panel: Rect, row: Int): Option[Int] =
      val offset = row - (panel.row + 2)
      if offset >= 0 && offset < themePresets.size then Some(offset) else None

    private def borderIndexAtRow(panel: Rect, row: Int): Option[Int] =
      val offset = row - (panel.row + 2)
      if offset >= 0 && offset < borderStyles.size then Some(offset) else None

    /**
     * Map a click `(col, row)` to a tab index when it lands on the Tabs
     * bar at row 2. Returns `None` when the click is on a separator,
     * outside the bar, or on a different row.
     */
    private def tabHitTest(col: Int, row: Int): Option[Int] =
      if row != tabsRow then None
      else widgets.Tabs.hitTest(showcaseTabs, colOffset = col - tabsCol)

    private def themesRect(m: Model): Rect =
      Rect(col = m.width - themesPanelWidth, row = topRowY, width = themesPanelWidth, height = topPanelHeight)

    private def bordersRect(m: Model): Rect =
      Rect(
        col = m.width - themesPanelWidth - bordersPanelWidth - 1,
        row = topRowY,
        width = bordersPanelWidth,
        height = topPanelHeight
      )

    private def capsRect: Rect =
      Rect(col = 1, row = topRowY, width = capsPanelWidth, height = topPanelHeight)

    private def paletteRect(m: Model): Rect =
      val left  = capsRect.col + capsRect.width + 1
      val right = bordersRect(m).col - 1
      Rect(col = left, row = topRowY, width = math.max(10, right - left), height = topPanelHeight)

    private def stylesRect: Rect =
      Rect(col = 1, row = bottomRowY, width = stylesPanelWidth, height = bottomPanelHeight)

    private def liveInputRect(m: Model): Rect =
      Rect(
        col = m.width - liveInputPanelWidth,
        row = bottomRowY,
        width = liveInputPanelWidth,
        height = bottomPanelHeight
      )

    private def unicodeRect(m: Model): Rect =
      val left  = stylesRect.col + stylesRect.width + 1
      val right = liveInputRect(m).col - 1
      Rect(col = left, row = bottomRowY, width = math.max(20, right - left), height = bottomPanelHeight)

    /**
     * One-line description of an [[KeyDecoder.InputKey]] for the "Live
     * input" panel. Mouse / paste / modified keys get distinct prefixes so
     * the Stage 2 wiring is visible at a glance.
     */
    private def formatEvent(k: KeyDecoder.InputKey): String =
      import KeyDecoder.InputKey.*
      k match
        case CharKey(c)            => s"key  CharKey('${c}')"
        case Ctrl(c)               => s"key  Ctrl+${c}"
        case Modified(inner, mods) => s"key  ${describeMods(mods)}+${describeKey(inner)}"
        case Paste(text) =>
          val preview = text.take(40).replace("\n", "\\n").replace("\r", "\\r")
          val suffix  = if text.length > 40 then "…" else ""
          s"paste(${text.length} chars) \"$preview$suffix\""
        case Mouse(ev) => s"mouse ${describeMouse(ev)}"
        case other     => s"key  ${describeKey(other)}"

    private def describeKey(k: KeyDecoder.InputKey): String =
      import KeyDecoder.InputKey.*
      k match
        case CharKey(c) => s"'$c'"
        case Ctrl(c)    => s"Ctrl+$c"
        case other      => other.toString.takeWhile(_ != '(')

    private def describeMods(m: KeyDecoder.Modifiers): String =
      val parts = List(
        Option.when(m.ctrl)("Ctrl"),
        Option.when(m.alt)("Alt"),
        Option.when(m.shift)("Shift"),
        Option.when(m.meta)("Meta")
      ).flatten
      if parts.isEmpty then "" else parts.mkString("+")

    private def describeMouse(ev: MouseEvent): String =
      val (col, row) = ev.at
      val mods       = describeMods(ev.modifiers)
      val modPrefix  = if mods.isEmpty then "" else s"$mods+"
      ev match
        case MouseEvent.Press(b, _, _, _)   => s"${modPrefix}${b}-press   ($col,$row)"
        case MouseEvent.Release(b, _, _, _) => s"${modPrefix}${b}-release ($col,$row)"
        case MouseEvent.Drag(b, _, _, _)    => s"${modPrefix}${b}-drag    ($col,$row)"
        case MouseEvent.Move(_, _, _)       => s"${modPrefix}move           ($col,$row)"
        case MouseEvent.Scroll(d, _, _, _)  => s"${modPrefix}scroll-$d  ($col,$row)"

    override def view(m: Model): RootNode =
      given Theme = m.theme

      val titleNode = TextNode(
        2.x,
        1.y,
        List(
          Text(
            "TermFlow Showcase",
            Style(fg = m.theme.primary, bold = true, italic = true)
          ),
          "  ".text,
          s"theme=${m.themeName}".text,
          "  ".text,
          s"border=${m.borderName}".text,
          "  ".text,
          s"size=${m.width}×${m.height}".text
        )
      )

      // Tab-aware help footer — common bindings on the left, then per-tab
      // hints on the right.
      val tabSwitch = List[Text](
        " 1..6 ".themed(_.primary),
        "switch tab  ".text,
        " click tab ".themed(_.primary),
        "  ".text
      )
      val perTab: List[Text] = m.activeTab match
        case WidgetsTabIdx =>
          List(
            " ↑/↓ ".themed(_.primary),
            "move  ".text,
            " Space ".themed(_.primary),
            "toggle  ".text
          )
        case InputsTabIdx =>
          List(
            " Tab ".themed(_.primary),
            "focus  ".text,
            " Enter ".themed(_.primary),
            "submit  ".text,
            " Space ".themed(_.primary),
            "toggle  ".text
          )
        case DataTabIdx =>
          List(
            " ↑/↓ ".themed(_.primary),
            "move  ".text,
            " Tab ".themed(_.primary),
            "focus  ".text
          )
        case LayoutTabIdx =>
          List(
            " ←/→ ".themed(_.primary),
            "menu  ".text,
            " ↓ ".themed(_.primary),
            "open  ".text,
            " [ / ] ".themed(_.primary),
            "split  ".text
          )
        case HelpTabIdx =>
          List(" press 1..5 to leave Help ".themed(_.info), "  ".text)
        case _ =>
          List(
            " b ".themed(_.primary),
            "border  ".text,
            " t ".themed(_.primary),
            "theme  ".text,
            " d ".themed(_.primary),
            "confirm  ".text,
            " i ".themed(_.primary),
            "input  ".text,
            " l ".themed(_.primary),
            "list  ".text,
            " w ".themed(_.primary),
            "wait  ".text,
            " f ".themed(_.primary),
            "file  ".text,
            " g ".themed(_.primary),
            "dir  ".text
          )
      val tail     = List[Text](" q ".themed(_.primary), "quit ".text)
      val helpNode = TextNode(2.x, (m.height - 1).y, tabSwitch ++ perTab ++ tail)

      // Interactive Tabs bar at row 2 — click a cell or press 1/2/3 to
      // switch the body. The hit-test in mouseDispatch / tabBarMouseDispatch
      // routes clicks against the same `showcaseTabs` labels.
      val tabsNode = widgets.Tabs(
        labels = showcaseTabs,
        activeIndex = m.activeTab,
        at = Coord(tabsCol.x, tabsRow.y)
      )

      val body: List[VNode] = m.activeTab match
        case WidgetsTabIdx   => widgetsTabBody(m)
        case InputsTabIdx    => inputsTabBody(m)
        case DataTabIdx      => dataTabBody(m)
        case LayoutTabIdx    => layoutTabBody(m)
        case WizardTabIdx    => wizardTabBody(m)
        case DashboardTabIdx => dashboardTabBody(m)
        case HelpTabIdx      => helpTabBody(m)
        case _               => showcaseTabBody(m)

      val baseRoot = RootNode(
        width = math.max(60, m.width),
        height = math.max(20, m.height),
        children = (titleNode :: tabsNode :: body) ++ List(helpNode),
        input = None
      )

      m.dialog match
        case Dialog.None => baseRoot
        case Dialog.ConfirmQuit(yesFocused) =>
          baseRoot.copy(overlays =
            List(
              Dialogs.confirm(
                prompt = "Quit the showcase?",
                yesFocused = yesFocused,
                title = "Confirm",
                yesLabel = "Quit",
                noLabel = "Stay"
              )
            )
          )
        case Dialog.TextInput(state, okFocused) =>
          baseRoot.copy(overlays =
            List(
              Dialogs.textInput(
                title = "Text input demo",
                prompt = "Type a value, Enter to accept:",
                value = state.buffer.mkString,
                cursor = state.cursor,
                okFocused = okFocused
              )
            )
          )
        case Dialog.ListSelect(idx) =>
          baseRoot.copy(overlays = List(buildListSelectOverlay(idx)))
        case Dialog.Waiting(openedAt, deadline) =>
          val remaining = math.max(0L, deadline - m.tick)
          baseRoot.copy(overlays =
            List(
              Dialogs.waiting(
                title = "Working…",
                body = s"simulated task — ${remaining / 10}.${remaining % 10}s remaining",
                tick = m.tick - openedAt,
                cancelLabel = Some("Cancel")
              )
            )
          )
        case Dialog.FilePicker(FilePickerMode.Files, p, entries, idx) =>
          baseRoot.copy(overlays =
            List(
              Dialogs.fileDialog(
                title = "Open file",
                currentPath = p,
                entries = entries,
                selectedIndex = idx,
                maxVisible = 8
              )
            )
          )
        case Dialog.FilePicker(FilePickerMode.Directories, p, entries, idx) =>
          baseRoot.copy(overlays =
            List(
              Dialogs.directoryDialog(
                title = "Choose directory",
                currentPath = p,
                entries = entries,
                selectedIndex = idx,
                maxVisible = 8
              )
            )
          )

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation("showcase has no prompt"))

    // ---- panels --------------------------------------------------------------

    private def capabilitiesPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val caps  = m.capabilities
      val title = TextNode(2.x, 1.y, List(" Capabilities ".themed(_.primary)))
      val rows = List(
        kv(2, 3, "depth", depthLabel(caps.colorDepth)),
        kv(2, 4, "mouse", yesNo(caps.mouse)),
        kv(2, 5, "utf-8", yesNo(caps.unicode)),
        kv(2, 6, "styles+", yesNo(caps.extendedStyles)),
        kv(2, 7, "paste", yesNo(caps.bracketedPaste)),
        TextNode(2.x, 9.y, List("$TERM=".text, m.termName.themed(_.info)))
      )
      panel(r, theme, title :: rows)

    private def palettePanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Palette ".themed(_.primary)))
      val description = TextNode(
        2.x,
        3.y,
        List("RGB swatches downgrade ".text, s"to ${depthLabel(m.capabilities.colorDepth)}".themed(_.info))
      )
      val swatch = TextNode(
        2.x,
        5.y,
        swatchRgb.toList.flatMap(c => List("███".text(fg = c), " ".text))
      )
      panel(r, theme, List(title, description, swatch))

    private def themesPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Themes (click) ".themed(_.primary)))
      // Use the RadioGroup widget instead of hand-rolled rows so the
      // showcase exercises the real widget API.
      val rows = widgets.RadioGroup(
        options = themePresets.map(_._1),
        selectedIndex = m.themeIdx,
        focusedIndex = m.themeIdx,
        at = Coord(2.x, 3.y)
      )
      val hint = TextNode(2.x, (3 + themePresets.size + 1).y, List("scroll to cycle".themed(_.info)))
      panel(r, theme, (title :: rows) :+ hint)

    private def bordersPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Borders (click) ".themed(_.primary)))
      val rows = widgets.RadioGroup(
        options = borderStyles.map(_._1),
        selectedIndex = m.borderIdx,
        focusedIndex = m.borderIdx,
        at = Coord(2.x, 3.y)
      )
      val hint = TextNode(2.x, (3 + borderStyles.size + 1).y, List("scroll to cycle".themed(_.info)))
      panel(r, theme, (title :: rows) :+ hint)

    private def stylesPanel(r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Styles ".themed(_.primary)))
      val rows = List(
        TextNode(2.x, 3.y, List(Text("bold", Style(fg = theme.primary, bold = true)))),
        TextNode(2.x, 4.y, List(Text("italic", Style(fg = theme.primary, italic = true)))),
        TextNode(2.x, 5.y, List(Text("underline", Style(fg = theme.primary, underline = true)))),
        TextNode(2.x, 6.y, List(Text("dim", Style(fg = theme.primary, dim = true)))),
        TextNode(2.x, 7.y, List(Text("reverse", Style(fg = theme.primary, reverse = true)))),
        TextNode(2.x, 8.y, List(Text("strike", Style(fg = theme.primary, strikethrough = true)))),
        TextNode(2.x, 9.y, List(Text("blink", Style(fg = theme.primary, blink = true)))),
        // Quick CheckBox demo — three states rendered through the real
        // widget so visual differences (theme.success on the checked
        // marker, bold on focus) are visible at a glance.
        widgets.CheckBox(label = "off", checked = false, focused = false, at = Coord(2.x, 10.y)),
        widgets.CheckBox(label = "on", checked = true, focused = false, at = Coord(2.x, 11.y)),
        widgets.CheckBox(label = "focus", checked = true, focused = true, at = Coord(2.x, 12.y))
      )
      panel(r, theme, title :: rows)

    private def unicodePanel(r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Unicode width ".themed(_.primary)))
      val ruler = TextNode(2.x, 3.y, List("|0123456789|0123456789|0123456789|".themed(_.border)))
      val cjk   = TextNode(2.x, 4.y, List("中文 日本語 한국어 → CJK = 2 cells".text(fg = theme.info)))
      val full  = TextNode(2.x, 5.y, List("ＡＢＣＤＥ → fullwidth ASCII".text(fg = theme.info)))
      val emoji = TextNode(2.x, 6.y, List("hello 🎉 world 🚀 → emoji = 2 cells".text(fg = theme.info)))
      val mixed = TextNode(2.x, 7.y, List("a中b日c韓 → mixed lays out cleanly".text(fg = theme.info)))
      val hint  = TextNode(2.x, 9.y, List("Each glyph above lines up with the ruler.".themed(_.success)))
      panel(r, theme, List(title, ruler, cjk, full, emoji, mixed, hint))

    private def liveInputPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Live input ".themed(_.primary)))
      val intro = TextNode(2.x, 3.y, List("Recent events (LogView, scrollable):".text))
      // LogView for the rolling event history. Five rows fit cleanly under
      // the intro inside the 13-row bottom panel.
      val historyRows = widgets.LogView(
        lines = m.eventHistory,
        width = r.width - 4,
        height = 5,
        at = Coord(2.x, 4.y),
        style = Some(Style(fg = theme.success))
      )
      val txt  = m.lastTextInput.map(s => s"\"${truncate(s, 24)}\"").getOrElse("—")
      val pick = m.lastListPick.getOrElse("—")
      val file = m.lastFilePick.map(truncate(_, 26)).getOrElse("—")
      val results = List(
        TextNode(2.x, 10.y, List("Last text input: ".text, txt.themed(_.info))),
        TextNode(2.x, 11.y, List("Last list pick:  ".text, pick.themed(_.info))),
        TextNode(2.x, 12.y, List("Last file pick:  ".text, file.themed(_.info)))
      )
      panel(r, theme, (title :: intro :: historyRows) ++ results)

    private def truncate(s: String, max: Int): String =
      if s.length <= max then s else s.take(max - 1) + "…"

    /**
     * Body content for the main "Showcase" tab — the original 7-panel
     *  Stage 1 + 2 demo.
     */
    private def showcaseTabBody(m: Model)(using theme: Theme): List[VNode] = List(
      capabilitiesPanel(m, capsRect),
      palettePanel(m, paletteRect(m)),
      themesPanel(m, themesRect(m)),
      bordersPanel(m, bordersRect(m)),
      stylesPanel(stylesRect),
      unicodePanel(unicodeRect(m)),
      liveInputPanel(m, liveInputRect(m))
    )

    /**
     * Body content for the "Widgets" tab — a single full-width panel
     *  showing the Tree widget over a sample file tree, with arrow-key
     *  navigation and Space/Enter to expand/collapse.
     */
    private def widgetsTabBody(m: Model)(using theme: Theme): List[VNode] =
      val r     = widgetsTabRect(m)
      val title = TextNode(2.x, 1.y, List(" Tree widget demo ".themed(_.primary)))
      val intro = TextNode(2.x, 3.y, List("Arrow keys move; Space/Enter toggles a node.".text))
      val treeRows = widgets.Tree(
        roots = demoTree,
        expanded = m.treeExpanded,
        selectedIndex = m.treeSelected,
        render = (_: DemoNode).name,
        at = Coord(2.x, 5.y)
      )
      List(panel(r, theme, title :: intro :: treeRows))

    /**
     * Body content for the "Inputs" tab — TextField + Select +
     * Autocomplete + CheckBox + Button laid out via the [[Form]] widget.
     */
    private def inputsTabBody(m: Model)(using theme: Theme): List[VNode] =
      val r = widgetsTabRect(m)
      val title = TextNode(
        2.x,
        1.y,
        List(" Form widget — TextField + Select + Autocomplete + CheckBox + Button ".themed(_.primary))
      )
      val intro = TextNode(2.x, 3.y, List("Tab/Shift+Tab cycles focus.  Enter submits.".text))

      val fieldWidth  = 28
      val selectWidth = 16

      val rows: Vector[widgets.Form.Row] = Vector(
        widgets.Form.Row(
          InpNameId,
          "Name:",
          focused => widgets.TextField.view(m.inputsName, lineWidth = fieldWidth, focused = focused)
        ),
        widgets.Form.Row(
          InpBioId,
          "Bio:",
          focused => widgets.TextField.view(m.inputsBio, lineWidth = fieldWidth, focused = focused)
        ),
        widgets.Form.Row(
          InpCountryId,
          "Country:",
          focused => widgets.Select.view(m.inputsCountry, lineWidth = selectWidth, focused = focused),
          height = if m.inputsCountry.open then widgets.Select.maxHeight(m.inputsCountry.listState.visibleRows) else 1
        ),
        widgets.Form.Row(
          InpFruitId,
          "Fruit:",
          _ =>
            // Autocomplete is always-open, so it doesn't take a `focused`
            // parameter — wrap its node list into a BoxNode so the Form
            // row can place a single VNode.
            val parts = widgets.Autocomplete.view(m.inputsFruit, width = fieldWidth, maxVisible = 5)
            VNode.BoxNode(1.x, 1.y, fieldWidth, widgets.Autocomplete.height(m.inputsFruit, 5), parts, Style())
          ,
          height = widgets.Autocomplete.height(m.inputsFruit, 5)
        ),
        widgets.Form.Row(
          InpAgreeId,
          "Agree:",
          focused => widgets.CheckBox(label = "I accept", checked = m.inputsAgree, focused = focused)
        ),
        widgets.Form.Row(
          InpSubmitId,
          "",
          focused => widgets.Button(label = "Submit", focused = focused)
        )
      )

      val formNodes = widgets.Form.column(
        rows = rows,
        focusManager = m.inputsFocus,
        at = Coord(2.x, 5.y),
        labelWidth = 10,
        gap = 1
      )

      val submittedY = 5 + widgets.Form.totalHeight(rows, gap = 1) + 1
      val submitted = m.inputsSubmitted match
        case None    => TextNode(2.x, submittedY.y, List("(no submission yet)".themed(_.info)))
        case Some(s) => TextNode(2.x, submittedY.y, List("Submitted: ".themed(_.success), s.text))

      List(panel(r, theme, (title :: intro :: formNodes) :+ submitted))

    /**
     * Body content for the "Data" tab — ListView + Table side-by-side
     * with Spinner + ProgressBar in the header and a StatusBar pinned
     * to the bottom of the panel.
     */
    private def dataTabBody(m: Model)(using theme: Theme): List[VNode] =
      val r = widgetsTabRect(m)

      val title = TextNode(2.x, 1.y, List(" ListView + Table + Spinner + ProgressBar + StatusBar ".themed(_.primary)))

      // Animated header: spinner + progress bar driven by the existing tick.
      val barWidth    = math.max(10, math.min(40, r.width - 16))
      val spinnerNode = widgets.Spinner(widgets.Spinner.Braille, frame = m.tick.toInt, at = Coord(2.x, 3.y))
      val progressNode = widgets.ProgressBar(
        value = m.dataProgress,
        width = barWidth,
        at = Coord(5.x, 3.y)
      )

      val intro = TextNode(2.x, 5.y, List("Tab focuses the widget that consumes ↑/↓.".themed(_.info)))

      // ListView (left) — focused selection drives the Table cursor too.
      val listFocused  = m.dataFocus.isFocused(DataListId)
      val tableFocused = m.dataFocus.isFocused(DataTableId)
      val listNode = widgets.ListView.view(
        m.dataList,
        at = Coord(2.x, 7.y),
        lineWidth = 28,
        focused = listFocused
      )

      // Table (right) — render rows from the same items + paired statuses.
      val tableRows: Vector[(String, String)] =
        m.dataList.items.zipWithIndex.map { case (t, i) =>
          (t, dataStatuses(i % dataStatuses.size))
        }
      val tableState = widgets.Table.State
        .of(DataColumns, tableRows, visibleRows = 8, selectable = true)
        .copy(body = m.dataList.copy(items = tableRows))
      val tableNode = widgets.Table.view(
        tableState,
        at = Coord(34.x, 7.y),
        focused = tableFocused
      )

      // StatusBar pinned to the bottom of the panel showing live counts.
      val total   = m.dataList.items.size
      val curIdx  = m.dataList.selected + 1
      val statusY = r.height - 1
      val statusBar = widgets.StatusBar(
        left = " data ",
        center = s"item $curIdx / $total",
        right = f" tick=${m.tick}  progress=${m.dataProgress * 100}%3.0f%% ",
        width = math.max(20, r.width - 2),
        at = Coord(2.x, statusY.y)
      )

      List(panel(r, theme, List(title, spinnerNode, progressNode, intro, listNode, tableNode, statusBar)))

    /**
     * Body content for the "Layout" tab — SplitPane horizontally split
     * with a MenuBar in the left pane and decorative content in the
     * right pane. `[` / `]` move the divider.
     */
    private def layoutTabBody(m: Model)(using theme: Theme): List[VNode] =
      val r = widgetsTabRect(m)

      val title = TextNode(2.x, 1.y, List(" SplitPane + MenuBar ".themed(_.primary)))
      val intro = TextNode(
        2.x,
        3.y,
        List("←/→ move between menus.  ↓/Enter open.  Esc close.  [ / ] resize split.".themed(_.info))
      )

      val splitTop    = 5
      val splitHeight = math.max(8, r.height - splitTop - 2)
      val splitWidth  = math.max(40, r.width - 4)

      val panes = widgets.SplitPane(
        first = (at, w, _) => {
          val menuNodes = widgets.MenuBar(m.layoutMenu, at = Coord(at.x + 1, at.y + 1))
          val pickRow = TextNode(
            (at.x.value + 1).x,
            (at.y.value + 8).y,
            m.layoutLastPick match
              case None    => List("(pick a menu item)".themed(_.info))
              case Some(s) => List("Picked: ".themed(_.success), s.text)
          )
          val ratioRow = TextNode(
            (at.x.value + 1).x,
            (at.y.value + 10).y,
            List(f"split ratio = ${m.layoutSplitRatio}%.2f".themed(_.foreground))
          )
          val _ = w
          menuNodes :+ pickRow :+ ratioRow
        },
        second = (at, w, _) => {
          val rowNodes = List(
            TextNode((at.x.value + 1).x, (at.y.value + 1).y, List(" Right pane ".themed(_.primary))),
            TextNode((at.x.value + 1).x, (at.y.value + 3).y, List("This pane is rendered by SplitPane.".text)),
            TextNode((at.x.value + 1).x, (at.y.value + 4).y, List(s"Width = $w cells.".themed(_.info))),
            TextNode((at.x.value + 1).x, (at.y.value + 6).y, List("[ shrinks the left pane".text)),
            TextNode((at.x.value + 1).x, (at.y.value + 7).y, List("] grows the left pane".text)),
            TextNode((at.x.value + 1).x, (at.y.value + 9).y, List("Drag-resize is on the".text)),
            TextNode((at.x.value + 1).x, (at.y.value + 10).y, List("Stage 3 mouse follow-up.".themed(_.info)))
          )
          rowNodes
        },
        width = splitWidth,
        height = splitHeight,
        direction = widgets.SplitPane.Direction.Horizontal,
        at = Coord(2.x, splitTop.y),
        splitRatio = m.layoutSplitRatio,
        gap = 1
      )

      // Visualize the divider so the split is obvious to the eye.
      val dividerNodes = widgets.SplitPane
        .dividerRect(
          direction = widgets.SplitPane.Direction.Horizontal,
          width = splitWidth,
          height = splitHeight,
          at = Coord(2.x, splitTop.y),
          splitRatio = m.layoutSplitRatio,
          gap = 1
        )
        .toList
        .flatMap { p =>
          (0 until p.height).map { row =>
            TextNode(p.at.x, (p.at.y.value + row).y, List(theme.chars.vertical.toString.themed(_.border)))
          }
        }

      List(panel(r, theme, (title :: intro :: panes) ++ dividerNodes))

    /** Body content for the "Help" tab — keybinding reference + about info. */
    private def helpTabBody(m: Model)(using theme: Theme): List[VNode] =
      val r     = widgetsTabRect(m)
      val title = TextNode(2.x, 1.y, List(" Keybindings & help ".themed(_.primary)))
      val rows = List(
        TextNode(2.x, 3.y, List(" 1..8 ".themed(_.primary), "  switch tab".text)),
        TextNode(2.x, 5.y, List(" 1 Showcase".themed(_.success))),
        TextNode(2.x, 6.y, List("   b  cycle border style".text)),
        TextNode(2.x, 7.y, List("   t  cycle theme".text)),
        TextNode(2.x, 8.y, List("   d / i / l / w  open dialogs".text)),
        TextNode(2.x, 9.y, List("   f / g  open fileDialog / directoryDialog".text)),
        TextNode(2.x, 10.y, List(" 2 Widgets (Tree)".themed(_.success))),
        TextNode(2.x, 11.y, List("   ↑/↓ move,  Space/Enter toggle node".text)),
        TextNode(2.x, 13.y, List(" 3 Inputs (Form)".themed(_.success))),
        TextNode(2.x, 14.y, List("   Tab/Shift+Tab cycle focus,  Enter submit".text)),
        TextNode(2.x, 15.y, List("   Space toggles the CheckBox / activates Button".text)),
        TextNode(2.x, 17.y, List(" 4 Data (List + Table)".themed(_.success))),
        TextNode(2.x, 18.y, List("   ↑/↓ move,  Tab focuses ListView vs Table".text)),
        TextNode(2.x, 20.y, List(" 5 Layout (SplitPane + MenuBar)".themed(_.success))),
        TextNode(2.x, 21.y, List("   ←/→ menu,  ↓ open,  Enter pick,  Esc close".text)),
        TextNode(2.x, 22.y, List("   [ / ] resize the split".text)),
        TextNode(2.x, 24.y, List(" 6 Wizard (3-step form)".themed(_.success))),
        TextNode(2.x, 25.y, List("   Tab/Shift+Tab cycle focus,  Enter activates Next/Submit".text)),
        TextNode(2.x, 26.y, List(" 7 Dashboard (live metrics)".themed(_.success))),
        TextNode(2.x, 27.y, List("   ↑/↓ select service,  r restart,  p / Space pause".text)),
        TextNode(2.x, 29.y, List(" Anywhere".themed(_.success))),
        TextNode(2.x, 30.y, List("   q / Esc  open quit confirmation".text)),
        TextNode(2.x, 31.y, List("   click on Tabs bar  switch tab".text))
      )
      List(panel(r, theme, title :: rows))

    /**
     * Body content for the "Wizard" tab — embeds [[WizardApp]]'s view by
     * rendering its `RootNode` and translating the children into the
     * tab's rect, so we don't need a parallel render path.
     */
    private def wizardTabBody(m: Model)(using theme: Theme): List[VNode] =
      val r        = widgetsTabRect(m)
      val embedded = WizardApp.App.view(m.wizardModel)
      embedded.children.map(c => Layout.translate(c, dx = r.col - 1, dy = r.row - 1))

    /**
     * Body content for the "Dashboard" tab — same delegation pattern as
     * `wizardTabBody`. The `Sub.Every` ticker on the showcase drives the
     * embedded simulation forward when this tab is active (see
     * `onTick`).
     */
    private def dashboardTabBody(m: Model)(using theme: Theme): List[VNode] =
      val r        = widgetsTabRect(m)
      val embedded = DashboardApp.App.view(m.dashboardModel)
      embedded.children.map(c => Layout.translate(c, dx = r.col - 1, dy = r.row - 1))

    /**
     * Rect that fills the area below the Tabs bar to the help footer.
     *  Leaves a 1-cell margin on the right (matching the Showcase tab's
     *  rightmost panel layout) so the box's right border doesn't get
     *  clipped or scroll the terminal.
     */
    private def widgetsTabRect(m: Model): Rect =
      val top    = topRowY
      val bottom = math.max(top + 5, m.height - 2) // leave row m.height-1 for footer
      val width  = math.max(60, m.width) - 1
      Rect(col = 1, row = top, width = width, height = bottom - top + 1)

    /** Bordered panel positioned at `r` with the theme's border style. */
    private def panel(r: Rect, theme: Theme, children: List[VNode]): VNode =
      VNode.BoxNode(
        x = r.col.x,
        y = r.row.y,
        width = r.width,
        height = r.height,
        children = children.map(translateInto(r, _)),
        style = Style(fg = theme.border, border = true),
        chars = theme.chars
      )

    /**
     * Translate a child authored in panel-local (1.x, 1.y) coordinates into
     *  the panel's absolute frame position.
     */
    private def translateInto(r: Rect, child: VNode): VNode =
      Layout.translate(child, r.col - 1, r.row - 1)

    private def kv(col: Int, row: Int, label: String, value: String)(using theme: Theme): VNode =
      TextNode(col.x, row.y, List(s"$label: ".text, value.themed(_.success)))

    private def yesNo(b: Boolean): String = if b then "yes" else "no"

    private def depthLabel(d: ColorDepth): String = d match
      case ColorDepth.Mono       => "Mono"
      case ColorDepth.Ansi8      => "Ansi8"
      case ColorDepth.Ansi16     => "Ansi16"
      case ColorDepth.Indexed256 => "Indexed256"
      case ColorDepth.Truecolor  => "Truecolor"
