# Forms and dialogs

The first three tutorials covered the runtime, state transitions, and
async work. The hard part of *real* TUI apps is everything that wraps
those primitives: focus management, validation, multi-step navigation,
and modal dialogs.

This tutorial walks through the **wizard** sample — a three-step
account-creation flow with per-step focus, per-field validation, a
radio-group, and a final review screen. Source:
[`termflow.apps.wizard.WizardApp`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/wizard/WizardApp.scala).
Run with `sbt wizardDemo`.

## What you'll build

```text
TermFlow Wizard
●  Account  ─  ○  Plan  ─  ○  Confirm

 Tab focus   Enter activate   ↑/↓ (Plan)   q quit

Name:    [Alice                ]
Email:   [alice@example.com    ]
                                      [ Next → ]
```

Three screens — Account, Plan, Confirm — connected by Next/Back
buttons. Tab cycles focus inside the current step; Enter activates the
focused control; arrow keys move the radio selection on the Plan step.
Validation errors appear inline next to fields when you try to advance
past Account with empty inputs.

## 1. The step state machine

A wizard is a state machine over its steps. Model the steps as an enum
and store the current one on the model:

```scala
enum Step:
  case Account, Plan, Confirm

val stepOrder: Vector[Step] = Vector(Step.Account, Step.Plan, Step.Confirm)
```

`stepOrder` is the canonical ordering — used both for the progress
indicator at the top and for next/back navigation. Don't compare
ordinals directly; query `stepOrder.indexOf(step)` so that adding a new
step in the middle is a one-line change.

## 2. Focus IDs per step

Each focusable control needs a `FocusId`. Group them by step so the
focus order on each screen is local:

```scala
// Account step
val NameId         = FocusId("wiz-name")
val EmailId        = FocusId("wiz-email")
val NextAccountId  = FocusId("wiz-account-next")
val accountFocusOrder = Vector(NameId, EmailId, NextAccountId)

// Plan step
val PlanRadioId  = FocusId("wiz-plan")
val BackPlanId   = FocusId("wiz-plan-back")
val NextPlanId   = FocusId("wiz-plan-next")
val planFocusOrder = Vector(PlanRadioId, BackPlanId, NextPlanId)

// Confirm step
val BackConfirmId  = FocusId("wiz-confirm-back")
val SubmitId       = FocusId("wiz-submit")
val confirmFocusOrder = Vector(BackConfirmId, SubmitId)
```

Two design choices worth noting:

- **`FocusId` is opaque over `String`.** You can't accidentally pass an
  arbitrary string where a `FocusId` is wanted, but two `FocusId`s
  built from the same string are equal — useful for testing.
- **String prefixes scope IDs.** `wiz-` keeps the wizard's IDs from
  colliding with other parts of the app when this gets embedded as a
  sub-component (which is exactly what the showcase tab does).

## 3. A `FocusManager` per step

The model carries one `FocusManager` per step:

```scala
final case class Model(
  step:       Step,
  name:       widgets.TextField.State,
  email:      widgets.TextField.State,
  planIndex:  Int,
  submitted:  Boolean,
  focus:      Map[Step, FocusManager],
  pendingKey: Option[KeyDecoder.InputKey],
  errors:     Map[String, String]
):
  def currentFocus: FocusManager = focus(step)
```

Why one per step instead of one global manager?

- **Stable per-screen focus.** When the user steps from Plan back to
  Account, focus returns to whichever field they last touched, not to
  the screen-zero default.
- **Clean tab cycle.** Tab on the Plan screen mustn't loop through
  Account fields. Per-step orders make this fall out naturally.

The `currentFocus` helper just looks up the manager for the active
step. Every focus-related operation in `update` calls it.

## 4. Pure update via `step(model, msg)`

This sample factors update into a pure `step(m, msg): Model` function
that the embeddable showcase reuses:

```scala
def step(m: Model, msg: Msg): Model =
  msg match
    case NextStep    => /* validate, advance */
    case PrevStep    => /* go back */
    case NextFocus   => m.copy(focus = m.focus.updated(m.step, m.currentFocus.next))
    case PrevFocus   => m.copy(focus = m.focus.updated(m.step, m.currentFocus.previous))
    case Activate    => stepActivate(m)
    case PlanUp      => m.copy(planIndex = math.max(0, m.planIndex - 1))
    case PlanDown    => m.copy(planIndex = math.min(planOptions.size - 1, m.planIndex + 1))
    case Submitted   => m.copy(submitted = true)
    case Key(k)      => stepKey(m, k)
    case _           => m
```

Then the runtime `update` becomes a thin shim that delegates to `step`
for everything except quit:

```scala
override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Quit        => Tui(m, Cmd.Exit)
    case Key(k)      => /* check quit-on-q-when-not-in-text-field, else delegate */
    case _           => step(m, msg).tui
```

This factoring is the *embeddable wizard* pattern: the model, the
messages, and the pure transition function are all reusable; only the
runtime glue ties to `Cmd.Exit`. The wizard ships as a tab inside the
showcase by reusing `step` directly.

## 5. Per-step validation

Validation is a pure function from `Model` to a `Map[String, String]`
keyed by `FocusId.value`:

```scala
def validateAccount(m: Model): Map[String, String] =
  val builder = Map.newBuilder[String, String]
  if m.name.buffer.trim.isEmpty then
    builder += (NameId.value -> "Name is required")
  val emailRaw = m.email.buffer.trim
  if emailRaw.isEmpty then
    builder += (EmailId.value -> "Email is required")
  else if !emailRaw.contains("@") then
    builder += (EmailId.value -> "Email must contain '@'")
  builder.result()
```

The keying-by-`FocusId.value` matters because `widgets.Form.column`
takes the same map shape — error text is drawn next to the field with
that id. No extra wiring.

`NextStep` consumes the result:

```scala
case NextStep =>
  m.step match
    case Step.Account =>
      val errs = validateAccount(m)
      if errs.nonEmpty then m.copy(errors = errs)
      else m.copy(step = Step.Plan, errors = Map.empty)
    case Step.Plan    => m.copy(step = Step.Confirm, errors = Map.empty)
    case Step.Confirm => m
```

The pattern is **validate-on-advance**: errors don't appear while
typing, only when the user tries to move forward with bad data. Less
noisy, gives the user space to think.

## 6. Focus dispatch

The single most important pattern in this sample:

```scala
private def stepKeyForStep(m: Model, k: KeyDecoder.InputKey): Model =
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
    /* … other steps … */
```

A keystroke is **dispatched by current focus**:

- If a `TextField` is focused, the key feeds through
  `TextField.handleKey` and updates the corresponding field.
- If a button is focused, the key activates it (`Enter`/`Space`) or
  navigates focus (arrow keys).

Two implementation details that matter:

- **`TextField.handleKey` returns `(nextState, Option[Msg])`** — the
  same shape as `Prompt.handleKey`. The widget owns its cursor and
  buffer. Mapping the optional Msg through `_ => None` says "this
  TextField never produces a Submit message" — pressing Enter inside
  the field is just a literal character.
- **Top-level Tab dispatch** is in `stepKey`, not here:
  ```scala
  k match
    case Tab     => step(m, NextFocus)
    case BackTab => step(m, PrevFocus)
    case _       => stepKeyForStep(m, k)
  ```
  Tab is *always* focus navigation, regardless of which control owns
  focus. It's tempting to put Tab handling inside each focus arm; one
  central handler keeps it consistent.

## 7. Quit semantics — quit-when-not-in-text-field

```scala
private def stepKey(m: Model, k: KeyDecoder.InputKey): Model =
  val isQuitKey = k match
    case CharKey('q') | CharKey('Q') | Escape => !m.isFocusedTextField
    case _                                    => false
  if isQuitKey then m
  else /* … */
```

`q` quits — except when the user is typing into a TextField, where it's
just the letter q. The `isFocusedTextField` helper checks the current
focus:

```scala
def isFocusedTextField: Boolean =
  step == Step.Account && (currentFocus.isFocused(NameId) || currentFocus.isFocused(EmailId))
```

This is one of those small details that makes a TUI feel professional.
Without it, typing `quentin@example.com` into the Email field would
quit the wizard halfway through.

## 8. Building forms with `widgets.Form.column`

The Account screen is rendered with the `Form.column` helper:

```scala
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
    rows         = rows,
    focusManager = m.currentFocus,
    at           = Coord(2.x, 6.y),
    labelWidth   = 8,
    gap          = 1,
    errors       = m.errors
  )
```

Each `Form.Row` is `(focusId, label, focused => VNode)`. The
`focused => VNode` closure is a small but powerful trick — the row
doesn't *know* whether it owns focus until `Form.column` resolves it
against the focus manager. That keeps focus state out of every widget's
constructor.

`Form.column` then:

- Looks up `focusManager.isFocused(row.focusId)` for each row.
- Calls `row.viewFor(focused)` to get the actual widget.
- Stacks rows vertically with `gap` between them.
- For each `errors.get(row.focusId.value)`, draws the error in red
  beneath the row.

## 9. The Plan step — RadioGroup

```scala
val radio = widgets.RadioGroup(
  options       = planOptions,
  selectedIndex = m.planIndex,
  focusedIndex  = m.planIndex,
  at            = Coord(4.x, 8.y)
)
```

`RadioGroup` is a stateless renderer — you pass the option list, the
selected index, and the focused index. State updates happen in
`update`:

```scala
case Some(id) if id == PlanRadioId =>
  k match
    case ArrowUp   => step(m, PlanUp)
    case ArrowDown => step(m, PlanDown)
    case Enter | CharKey(' ') =>
      // Commit the selection and skip the Back button —
      // the user is advancing, not retreating.
      m.copy(focus = m.focus.updated(m.step, m.currentFocus.focus(NextPlanId)))
    case _ => m
```

The `focus(NextPlanId)` call **explicitly sets focus to a specific id**
— equivalent to "Tab past Back, land on Next". This skips the Back
button in the focus order because the user just committed to advance.
A small touch that turns radio + button into a smooth one-keystroke
flow.

## 10. The Confirm step — read-only summary + Submit

```scala
private def confirmStep(m: Model)(using theme: Theme): List[VNode] =
  val title    = TextNode(2.x, 6.y, List("Review:".themed(_.primary)))
  val summary  = m.summary.zipWithIndex.map { case (line, i) =>
    TextNode(4.x, (8 + i).y, List(line.text))
  }
  val backBtn  = widgets.Button(label = "← Back", focused = m.currentFocus.isFocused(BackConfirmId))
  val submit   = widgets.Button(label = "Submit", focused = m.currentFocus.isFocused(SubmitId))
  /* … plus a "✓ Submitted!" line if m.submitted … */
```

Notes:

- **`m.summary`** is a tiny pure helper on `Model` that returns the
  three review lines as `List[String]`. Keeping it on the model makes
  it trivially testable.
- **`Layout.translate(node, dx, dy)`** offsets a sub-node — a quick way
  to position two buttons side-by-side on the same row.
- **The submitted state is just a flag.** Once `Submit` runs, the
  model's `submitted = true` and `view` adds a confirmation line. We
  don't navigate away — the user reads "Submitted!" then presses `q`
  to quit.

## 11. Step indicator at the top

```scala
private def renderStepIndicator(m: Model)(using theme: Theme): List[Text] =
  stepOrder.zipWithIndex.flatMap { case (s, i) =>
    val visited = i <= m.stepIndex
    val current = i == m.stepIndex
    val glyph   = if visited then "●" else "○"
    val style   =
      if current then Style(fg = theme.primary, bold = true)
      else if visited then Style(fg = theme.success)
      else Style(fg = theme.foreground)
    val sep = if i == stepOrder.size - 1 then "" else "  ─  "
    List(Text(s"$glyph ", style), Text(stepLabel(s), style), Text(sep, Style(fg = theme.border)))
  }.toList
```

A trio of glyph / label / separator per step, joined into a single
`List[Text]` that goes inside one `TextNode`. The progress shape —
filled-and-bold for the current step, filled-and-coloured for visited,
empty for upcoming — is the kind of detail you can swap to your taste
without touching the logic.

## What you've learned

- **`FocusManager` per screen** keeps tab cycles local and lets each
  screen restore its own focus when revisited.
- **Focus dispatch in `update`** is the central pattern — the focused
  widget id determines how a keystroke is interpreted.
- **`widgets.Form.column`** turns rows of `(focusId, label, viewFor)`
  into a styled, focus-aware, error-decorated form.
- **Validate-on-advance, not on every keystroke.** Map `FocusId.value`
  to error strings; pass the same map to `Form.column`.
- **Quit-when-not-in-text-field** is what makes `q` feel right.
- **Pure `step(model, msg)`** factors the wizard so it can be embedded
  somewhere else (the showcase) without dragging the runtime in.

## Beyond the tutorial

The wizard is just the start of what `widgets.Form` and the dialog
helpers can do. Keep going:

- **Modal dialogs.** `Dialogs.confirm` / `Dialogs.textInput` /
  `Dialogs.listSelect` / `Dialogs.fileDialog` — see the
  [showcase app](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/showcase/Stage1ShowcaseApp.scala)'s
  Dialogs tab for every variant.
- **`Dialogs.actionList(title, actions, position)`** — a vertical menu
  of `Choice` items with mouse hit-testing.
- **Keymaps.** Replace ad-hoc pattern matches with `Keymap.focus`
  bindings — see the [Keymap guide](../guide/keymap.md) (stub for now).
- **Multi-line input.** `widgets.MultiLineInput` for full editor
  embedding — see the showcase's Editor tab.

That wraps the four tutorials. Heading back to the
[user guide index](../intro/what-is-termflow.md) is a good next step,
or jump straight into the [Widgets guide](../guide/widgets.md) for the
full component catalogue.
