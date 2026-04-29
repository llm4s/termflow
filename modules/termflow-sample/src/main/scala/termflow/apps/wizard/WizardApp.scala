package termflow.apps.wizard

import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets

/**
 * Three-step wizard sample (Stage 4 §7.3) demonstrating multi-step forms
 * with back / forward navigation, per-step validation, and a summary on
 * the final step.
 *
 * Steps:
 *   1. Account — name + email (both required)
 *   2. Plan — radio select between Free / Pro / Enterprise
 *   3. Confirm — read-only summary plus Submit / Back buttons
 *
 * The step indicator at the top renders as `● ─ ● ─ ○`, with `●` for
 * completed / current and `○` for upcoming. Forward progression is
 * gated on validation; the per-row error map drawn by `Form.column`
 * surfaces what's missing without leaving the page.
 *
 * Demonstrates: `Form.column`, `FocusManager`, `RadioGroup`, `Keymap`,
 * `Sub.InputKey`. Plus a small state machine (`Step` enum) over the
 * model that you'd find in any real wizard / installer flow.
 *
 * ## Keys
 *
 *   - `Tab` / `Shift+Tab`     cycle focus inside the active step
 *   - `↑` / `↓`               (Plan step) move radio selection
 *   - `Enter` / `Space`       activate the focused button
 *   - `Alt+→` / `Alt+←` / `n` / `b`  next / back without changing focus
 *   - `q` / `Esc` / `Ctrl+C`  quit
 *
 * Run with `sbt wizardDemo`.
 */
object WizardApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  /** The fixed step ordering. Index drives the progress indicator. */
  enum Step:
    case Account
    case Plan
    case Confirm

  val stepOrder: Vector[Step] = Vector(Step.Account, Step.Plan, Step.Confirm)

  /** Plan options for step 2. Stable order; index doubles as ordinal. */
  val planOptions: Vector[String] = Vector("Free", "Pro", "Enterprise")

  // ---- Focus IDs -----------------------------------------------------------
  // Account step
  val NameId: FocusId                    = FocusId("wiz-name")
  val EmailId: FocusId                   = FocusId("wiz-email")
  val NextAccountId: FocusId             = FocusId("wiz-account-next")
  val accountFocusOrder: Vector[FocusId] = Vector(NameId, EmailId, NextAccountId)

  // Plan step
  val PlanRadioId: FocusId            = FocusId("wiz-plan")
  val BackPlanId: FocusId             = FocusId("wiz-plan-back")
  val NextPlanId: FocusId             = FocusId("wiz-plan-next")
  val planFocusOrder: Vector[FocusId] = Vector(PlanRadioId, BackPlanId, NextPlanId)

  // Confirm step
  val BackConfirmId: FocusId             = FocusId("wiz-confirm-back")
  val SubmitId: FocusId                  = FocusId("wiz-submit")
  val confirmFocusOrder: Vector[FocusId] = Vector(BackConfirmId, SubmitId)

  enum Msg:
    case NextStep
    case PrevStep
    case NextFocus
    case PrevFocus
    case Activate
    case ToggleField
    case PlanUp
    case PlanDown
    case Submitted
    case Quit
    case Key(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg.*

  final case class Model(
    step: Step,
    name: widgets.TextField.State,
    email: widgets.TextField.State,
    planIndex: Int,
    submitted: Boolean,
    focus: Map[Step, FocusManager],
    /** Keystroke that should be folded into the focused TextField next, if any. */
    pendingKey: Option[KeyDecoder.InputKey],
    /** Validation messages for the current step keyed by `FocusId.value`. */
    errors: Map[String, String]
  ):
    def currentFocus: FocusManager = focus(step)
    def isFocusedTextField: Boolean =
      step == Step.Account && (currentFocus.isFocused(NameId) || currentFocus.isFocused(EmailId))

    /** Index of the current step in `stepOrder`. */
    def stepIndex: Int = stepOrder.indexOf(step)

    /** Read-only summary used by the Confirm step's review panel. */
    def summary: List[String] = List(
      s"Name:  ${if name.buffer.isEmpty then "—" else name.buffer}",
      s"Email: ${if email.buffer.isEmpty then "—" else email.buffer}",
      s"Plan:  ${planOptions(planIndex)}"
    )

  /**
   * Validation for the Account step. Keyed by `FocusId.value` so it drops
   * straight into `Form.column(errors = …)`. `Plan` and `Confirm` need no
   * validation — both have a default.
   */
  def validateAccount(m: Model): Map[String, String] =
    val builder = Map.newBuilder[String, String]
    if m.name.buffer.trim.isEmpty then builder += (NameId.value -> "Name is required")
    val emailRaw = m.email.buffer.trim
    if emailRaw.isEmpty then builder += (EmailId.value -> "Email is required")
    else if !emailRaw.contains("@") then builder += (EmailId.value -> "Email must contain '@'")
    builder.result()

  /**
   * Pure factory for the wizard's initial model. The full [[App]] runtime
   * uses this in `init` after subscribing to keyboard input; the
   * embeddable form lets the Stage 1 showcase construct a wizard model
   * without registering its own subscriptions.
   */
  def initialModel: Model =
    Model(
      step = Step.Account,
      name = widgets.TextField.State.withPlaceholder("Alice"),
      email = widgets.TextField.State.withPlaceholder("alice@example.com"),
      planIndex = 0,
      submitted = false,
      focus = Map(
        Step.Account -> FocusManager(accountFocusOrder),
        Step.Plan    -> FocusManager(planFocusOrder),
        Step.Confirm -> FocusManager(confirmFocusOrder)
      ),
      pendingKey = None,
      errors = Map.empty
    )

  /**
   * Pure model transition for one [[Msg]]. The full [[App]] runtime
   * delegates to this for everything except `Quit`, which it pairs with
   * `Cmd.Exit`. Exposed at object scope so the Stage 1 showcase can embed
   * the wizard as a tab without re-implementing the state machine.
   */
  def step(m: Model, msg: Msg): Model =
    msg match
      case NextStep =>
        m.step match
          case Step.Account =>
            val errs = validateAccount(m)
            if errs.nonEmpty then m.copy(errors = errs)
            else m.copy(step = Step.Plan, errors = Map.empty)
          case Step.Plan    => m.copy(step = Step.Confirm, errors = Map.empty)
          case Step.Confirm => m

      case PrevStep =>
        m.step match
          case Step.Account => m
          case Step.Plan    => m.copy(step = Step.Account, errors = Map.empty)
          case Step.Confirm => m.copy(step = Step.Plan, errors = Map.empty)

      case NextFocus   => m.copy(focus = m.focus.updated(m.step, m.currentFocus.next))
      case PrevFocus   => m.copy(focus = m.focus.updated(m.step, m.currentFocus.previous))
      case Activate    => stepActivate(m)
      case ToggleField => m.copy(focus = m.focus.updated(m.step, m.currentFocus.next))
      case PlanUp      => m.copy(planIndex = math.max(0, m.planIndex - 1))
      case PlanDown    => m.copy(planIndex = math.min(planOptions.size - 1, m.planIndex + 1))
      case Submitted   => m.copy(submitted = true)
      case Quit        => m
      case KeyError(_) => m
      case Key(k)      => stepKey(m, k)

  /** Activate the focused button — pure variant of `activateFocused`. */
  private def stepActivate(m: Model): Model =
    m.currentFocus.current match
      case Some(NextAccountId) => step(m, NextStep)
      case Some(BackPlanId)    => step(m, PrevStep)
      case Some(NextPlanId)    => step(m, NextStep)
      case Some(BackConfirmId) => step(m, PrevStep)
      case Some(SubmitId)      => step(m, Submitted)
      case _                   => m

  /** Pure key dispatch — mirrors `onKey` without the runtime side-effects. */
  private def stepKey(m: Model, k: KeyDecoder.InputKey): Model =
    import KeyDecoder.InputKey.*
    val isQuitKey = k match
      case CharKey('q') | CharKey('Q') | Escape => !m.isFocusedTextField
      case _                                    => false
    if isQuitKey then m // runtime handles Quit at the App layer
    else
      k match
        case Tab     => step(m, NextFocus)
        case BackTab => step(m, PrevFocus)
        case _       => stepKeyForStep(m, k)

  private def stepKeyForStep(m: Model, k: KeyDecoder.InputKey): Model =
    import KeyDecoder.InputKey.*
    m.step match
      case Step.Account =>
        m.currentFocus.current match
          case Some(id) if id == NameId =>
            val (next, _) = widgets.TextField.handleKey[Msg](m.name, k)(_ => None)
            m.copy(name = next)
          case Some(id) if id == EmailId =>
            val (next, _) = widgets.TextField.handleKey[Msg](m.email, k)(_ => None)
            m.copy(email = next)
          case Some(id) if id == NextAccountId =>
            k match
              case Enter | CharKey(' ') => step(m, NextStep)
              case ArrowLeft            => step(m, PrevFocus)
              case _                    => m
          case _ => m

      case Step.Plan =>
        m.currentFocus.current match
          case Some(id) if id == PlanRadioId =>
            k match
              case ArrowUp   => step(m, PlanUp)
              case ArrowDown => step(m, PlanDown)
              // Enter / Space commit the current selection (it's already
              // chosen via arrow keys) and jump focus straight to the
              // Next button — the user is committing to advance, not
              // backing out, so we skip the Back button in the focus
              // cycle.
              case Enter | CharKey(' ') =>
                m.copy(focus = m.focus.updated(m.step, m.currentFocus.focus(NextPlanId)))
              case _ => m
          case Some(id) if id == BackPlanId =>
            k match
              case Enter | CharKey(' ') => step(m, PrevStep)
              case ArrowRight           => step(m, NextFocus)
              case _                    => m
          case Some(id) if id == NextPlanId =>
            k match
              case Enter | CharKey(' ') => step(m, NextStep)
              case ArrowLeft            => step(m, PrevFocus)
              case _                    => m
          case _ => m

      case Step.Confirm =>
        m.currentFocus.current match
          case Some(id) if id == BackConfirmId =>
            k match
              case Enter | CharKey(' ') => step(m, PrevStep)
              case ArrowRight           => step(m, NextFocus)
              case _                    => m
          case Some(id) if id == SubmitId =>
            k match
              case Enter | CharKey(' ') => step(m, Submitted)
              case ArrowLeft            => step(m, PrevFocus)
              case _                    => m
          case _ => m

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Sub.InputKey(Key.apply, KeyError.apply, ctx)
      initialModel.tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val _ = ctx
      msg match
        // Quit is the only message with a non-NoCmd effect at the runtime
        // boundary; inside Key(k) we also need to exit on `q`/Esc when
        // not focused on a TextField. Everything else delegates to the
        // pure `step` function so the showcase can embed the wizard.
        case Quit => Tui(m, Cmd.Exit)
        case Key(k) =>
          import KeyDecoder.InputKey.*
          val quitKey = k match
            case CharKey('q') | CharKey('Q') | Escape => !m.isFocusedTextField
            case _                                    => false
          if quitKey then Tui(m, Cmd.Exit)
          else step(m, msg).tui
        case _ => step(m, msg).tui

    override def view(m: Model): RootNode =
      given Theme = Theme.dark

      val title = TextNode(
        2.x,
        1.y,
        List(
          Text("TermFlow Wizard", Style(fg = Theme.dark.primary, bold = true))
        )
      )
      val stepIndicator = TextNode(2.x, 2.y, renderStepIndicator(m))

      val help = TextNode(
        2.x,
        4.y,
        List(
          " Tab ".themed(_.primary),
          "focus  ".text,
          " Enter ".themed(_.primary),
          "activate  ".text,
          " ↑/↓ ".themed(_.primary),
          "(Plan)  ".text,
          " q ".themed(_.primary),
          "quit".text
        )
      )

      val body: List[VNode] = m.step match
        case Step.Account => accountStep(m)
        case Step.Plan    => planStep(m)
        case Step.Confirm => confirmStep(m)

      RootNode(
        width = 80,
        height = 24,
        children = title :: stepIndicator :: help :: body,
        input = None
      )

    /** Step indicator: one cell per step, joined by ─. Filled = visited. */
    private def renderStepIndicator(m: Model)(using theme: Theme): List[Text] =
      val cells = stepOrder.zipWithIndex.flatMap { case (s, i) =>
        val visited = i <= m.stepIndex
        val current = i == m.stepIndex
        val glyph =
          if current then "●"
          else if visited then "●"
          else "○"
        val style =
          if current then Style(fg = theme.primary, bold = true)
          else if visited then Style(fg = theme.success)
          else Style(fg = theme.foreground)
        val label = stepLabel(s)
        val sep   = if i == stepOrder.size - 1 then "" else "  ─  "
        List(
          Text(s"$glyph ", style),
          Text(label, style),
          Text(sep, Style(fg = theme.border))
        )
      }
      cells.toList

    private def stepLabel(s: Step): String = s match
      case Step.Account => "Account"
      case Step.Plan    => "Plan"
      case Step.Confirm => "Confirm"

    private def accountStep(m: Model)(using theme: Theme): List[VNode] =
      val rows = Vector(
        widgets.Form.Row(
          NameId,
          "Name:",
          focused => widgets.TextField.view(m.name, lineWidth = 28, focused = focused)
        ),
        widgets.Form.Row(
          EmailId,
          "Email:",
          focused => widgets.TextField.view(m.email, lineWidth = 28, focused = focused)
        ),
        widgets.Form.Row(
          NextAccountId,
          "",
          focused => widgets.Button(label = "Next →", focused = focused),
          height = 1
        )
      )
      widgets.Form.column(
        rows = rows,
        focusManager = m.currentFocus,
        at = Coord(2.x, 6.y),
        labelWidth = 8,
        gap = 1,
        errors = m.errors
      )

    private def planStep(m: Model)(using theme: Theme): List[VNode] =
      val intro = TextNode(2.x, 6.y, List("Choose a plan:".themed(_.primary)))
      val radio = widgets.RadioGroup(
        options = planOptions,
        selectedIndex = m.planIndex,
        focusedIndex = m.planIndex,
        at = Coord(4.x, 8.y)
      )
      val buttonsY = 8 + planOptions.size + 2
      val backBtn  = widgets.Button(label = "← Back", focused = m.currentFocus.isFocused(BackPlanId))
      val nextBtn  = widgets.Button(label = "Next →", focused = m.currentFocus.isFocused(NextPlanId))
      val backNode = Layout.translate(backBtn, dx = 1, dy = buttonsY - 1)
      val nextNode = Layout.translate(nextBtn, dx = 12, dy = buttonsY - 1)
      List(intro) ++ radio ++ List(backNode, nextNode)

    private def confirmStep(m: Model)(using theme: Theme): List[VNode] =
      val title = TextNode(2.x, 6.y, List("Review:".themed(_.primary)))
      val summary = m.summary.zipWithIndex.map { case (line, i) =>
        TextNode(4.x, (8 + i).y, List(line.text))
      }
      val buttonsY   = 8 + m.summary.size + 2
      val backBtn    = widgets.Button(label = "← Back", focused = m.currentFocus.isFocused(BackConfirmId))
      val submitBtn  = widgets.Button(label = "Submit", focused = m.currentFocus.isFocused(SubmitId))
      val backNode   = Layout.translate(backBtn, dx = 1, dy = buttonsY - 1)
      val submitNode = Layout.translate(submitBtn, dx = 12, dy = buttonsY - 1)
      val confirmation: List[VNode] =
        if m.submitted then
          List(
            TextNode(
              2.x,
              (buttonsY + 2).y,
              List(
                "✓ ".themed(_.success),
                Text("Submitted!", Style(fg = Theme.dark.success, bold = true)),
                "  press q to quit.".text
              )
            )
          )
        else Nil
      (title :: summary.toList) ++ List(backNode, submitNode) ++ confirmation

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation("Wizard has no prompt"))
