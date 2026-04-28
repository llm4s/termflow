package termflow.apps.dashboard

import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets

import scala.util.Random

/**
 * Multi-pane realtime dashboard sample (Stage 4 §7.3).
 *
 * Shows a fleet of fake services with a CPU bar each, ticking on a
 * `Sub.Every` timer. The selected service can be "restarted" with `r`,
 * dropping its load to zero; the simulation can be paused / resumed
 * with `p`. The header animates a `Spinner` while the simulation runs;
 * the footer is a `StatusBar` showing aggregate counters.
 *
 * Demonstrates `ListView`, `ProgressBar`, `Spinner`, `StatusBar`, plus
 * `Sub.Every` timers and `Sub.InputKey` keyboard input wired through a
 * declarative `Keymap`.
 *
 * ## Keys
 *
 *   - `↑` / `↓`              cycle the selected service
 *   - `r`                    restart the selected service (CPU → 0)
 *   - `p` / `Space`          pause / resume the simulation
 *   - `q` / `Esc` / `Ctrl+C` quit
 *
 * Run with `sbt dashboardDemo`.
 */
object DashboardApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  /** Default service catalogue. Stable order so the demo is reproducible. */
  val defaultServices: Vector[String] =
    Vector(
      "auth-service",
      "billing-service",
      "search-indexer",
      "image-resizer",
      "analytics-pipeline",
      "notification-bus",
      "session-store",
      "audit-log"
    )

  /**
   * One service row — a name, current CPU load (0..1), and a tick count
   * representing how many simulation steps the service has weathered
   * since its last restart. Pure data, no widgets here.
   */
  final case class ServiceState(name: String, cpu: Double, ticks: Long):
    def withCpu(next: Double): ServiceState = copy(cpu = math.max(0.0, math.min(1.0, next)))

  enum Msg:
    case Tick
    case Restart
    case TogglePause
    case Quit
    case Key(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg.*

  /**
   * Declarative key bindings. List navigation (↑/↓ / Home / End) is
   * routed through `ListView.handleKey` rather than the keymap so the
   * widget can update its own scroll offset; everything else dispatches
   * through this table.
   */
  val Keys: Keymap[Msg] =
    Keymap.quit(Quit) ++
      Keymap(
        KeyDecoder.InputKey.CharKey('r') -> Restart,
        KeyDecoder.InputKey.CharKey('R') -> Restart,
        KeyDecoder.InputKey.CharKey('p') -> TogglePause,
        KeyDecoder.InputKey.CharKey('P') -> TogglePause,
        KeyDecoder.InputKey.CharKey(' ') -> TogglePause
      )

  final case class Model(
    services: widgets.ListView.State[ServiceState],
    tick: Long,
    paused: Boolean,
    rng: Random
  ):
    /** Convenience: current service highlighted in the list, if any. */
    def selectedService: Option[ServiceState] = services.selectedItem

    /** Number of services running above the warning threshold. */
    def hotCount: Int = services.items.count(_.cpu >= 0.85)

  /**
   * Animation period. Slow enough to be readable, fast enough to feel
   * live. Same cadence the testkit uses to drive deterministic
   * simulation steps in `DashboardAppSpec`.
   */
  val tickPeriodMs: Long = 200L

  /** Visible rows in the service `ListView`. */
  val visibleServices: Int = 8

  /**
   * Pure factory for the dashboard's initial model. The full [[App]]
   * runtime uses this in `init` after wiring its `Sub.Every` ticker; the
   * embeddable form lets the Stage 1 showcase slot the dashboard into a
   * tab without registering its own subscriptions.
   */
  def initialModel: Model =
    val services = defaultServices.zipWithIndex.map { case (name, i) =>
      ServiceState(name, cpu = 0.10 + (i * 0.07) % 0.6, ticks = 0L)
    }
    Model(
      services = widgets.ListView.State.of(services, visibleRows = visibleServices),
      tick = 0L,
      paused = false,
      rng = new Random(20260428L)
    )

  /**
   * Pure model transition for one [[Msg]]. The full [[App]] runtime
   * delegates to this for everything except `Quit`, which it pairs with
   * `Cmd.Exit`. Exposed at object scope so the Stage 1 showcase can
   * embed the dashboard as a tab without re-implementing the simulation.
   */
  def step(m: Model, msg: Msg): Model =
    msg match
      case Tick =>
        if m.paused then m.copy(tick = m.tick + 1)
        else
          val nextItems = m.services.items.map { s =>
            val delta = (m.rng.nextDouble() - 0.5) * 0.20
            s.withCpu(s.cpu + delta).copy(ticks = s.ticks + 1)
          }
          m.copy(services = m.services.withItems(nextItems), tick = m.tick + 1)

      case Restart =>
        m.services.selectedItem match
          case None => m
          case Some(picked) =>
            val idx        = m.services.selected
            val resetItems = m.services.items.updated(idx, picked.copy(cpu = 0.0, ticks = 0L))
            m.copy(services = m.services.withItems(resetItems))

      case TogglePause => m.copy(paused = !m.paused)
      case Quit        => m
      case KeyError(_) => m
      case Key(k) =>
        val (nextList, _) = widgets.ListView.handleKey[ServiceState, Msg](m.services, k)(_ => None)
        val nm            = m.copy(services = nextList)
        Keys.lookup(k) match
          case Some(out) => step(nm, out)
          case None      => nm

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Sub.Every(tickPeriodMs, () => Tick, ctx)
      Sub.InputKey(Key.apply, KeyError.apply, ctx)
      initialModel.tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val _ = ctx
      msg match
        case Quit => Tui(m, Cmd.Exit)
        case Key(k) =>
          val (nextList, _) = widgets.ListView.handleKey[ServiceState, Msg](m.services, k)(_ => None)
          val nm            = m.copy(services = nextList)
          Keys.lookup(k) match
            case Some(Quit) => Tui(nm, Cmd.Exit)
            case Some(out)  => step(nm, out).tui
            case None       => nm.tui
        case _ => step(m, msg).tui

    override def view(m: Model): RootNode =
      given Theme = Theme.dark

      val title = TextNode(
        2.x,
        1.y,
        List(
          Text("TermFlow Dashboard", Style(fg = Theme.dark.primary, bold = true)),
          "  ".text,
          (if m.paused then "[paused]" else "[live]")
            .themed(if m.paused then _.warning else _.success)
        )
      )
      val spinner = widgets.Spinner(
        widgets.Spinner.Braille,
        frame = m.tick.toInt,
        at = Coord(36.x, 1.y)
      )

      val help = TextNode(
        2.x,
        2.y,
        List(
          " ↑/↓ ".themed(_.primary),
          "select  ".text,
          " r ".themed(_.primary),
          "restart  ".text,
          " p / Space ".themed(_.primary),
          "pause  ".text,
          " q ".themed(_.primary),
          "quit".text
        )
      )

      // Services list (left pane).
      val list = widgets.ListView.view[ServiceState](
        m.services,
        at = Coord(2.x, 4.y),
        lineWidth = 28,
        focused = true,
        render = s => f"${s.name}%-22s ${(s.cpu * 100).toInt}%3d%%"
      )

      // Metrics panel (right pane) — one ProgressBar per service.
      val metricsLabel = TextNode(34.x, 4.y, List(" CPU load ".themed(_.primary)))
      val barWidth     = 24
      val metricsRows = m.services.items.zipWithIndex.toList.map { case (svc, i) =>
        val rowY     = 5 + i
        val nameNode = TextNode(34.x, rowY.y, List(s"${svc.name}".text))
        val barColor =
          if svc.cpu >= 0.85 then Theme.dark.error
          else if svc.cpu >= 0.6 then Theme.dark.warning
          else Theme.dark.success
        val bar = widgets.ProgressBar(
          value = svc.cpu,
          width = barWidth,
          at = Coord((34 + 22).x, rowY.y)
        )(using Theme.dark.copy(primary = barColor))
        List(nameNode, bar)
      }.flatten

      val statusY = 5 + visibleServices + 1
      val statusBar = widgets.StatusBar(
        left = " dashboard ",
        center = f"${m.services.size} services • ${m.hotCount} hot • tick=${m.tick}",
        right = if m.paused then " paused " else " live ",
        width = 90,
        at = Coord(2.x, statusY.y)
      )

      val nodes = List(title, spinner, help, list, metricsLabel) ++ metricsRows :+ statusBar
      RootNode(
        width = 92,
        height = statusY + 2,
        children = nodes,
        input = None
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation("Dashboard has no prompt"))
