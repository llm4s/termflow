package termflow.apps.forms

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets.*

/**
 * Multi-field form demo wiring [[TextField]], [[FocusManager]], [[Keymap]],
 * and [[Layout]] together. Three editable fields (Name / Email / Bio) plus
 * Submit and Reset buttons share one focus order; Tab cycles through them
 * all and Enter does the right thing per element type.
 *
 * ## Keys
 *
 *   - `Tab`              cycle focus forward (Name → Email → Bio → Submit → Reset → Name)
 *   - `Enter` (in field) advance focus to the next element (keeps the typed text)
 *   - `Enter` (button)   activate Submit / Reset
 *   - `t`                toggle dark / light theme
 *   - `q` / `Ctrl+C` / `Esc` quit
 *
 * Run with:
 * {{{
 *   sbt formDemo
 *   // or:
 *   sbt "termflowSample/runMain termflow.apps.forms.FormDemoApp"
 * }}}
 */
object FormDemoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  // Focus identifiers — exported so tests can reference them by name.
  val NameId: FocusId   = FocusId("name")
  val EmailId: FocusId  = FocusId("email")
  val BioId: FocusId    = FocusId("bio")
  val SubmitId: FocusId = FocusId("submit")
  val ResetId: FocusId  = FocusId("reset")

  /** The Tab cycle: fields first, then the buttons. */
  val FocusOrder: Vector[FocusId] = Vector(NameId, EmailId, BioId, SubmitId, ResetId)

  /** Form result captured when Submit is activated. */
  final case class Submission(name: String, email: String, bio: String)

  enum Msg:
    case NextFocus
    case PrevFocus
    case Activate
    case ToggleTheme
    case Reset
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)
    case Quit

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    name: TextField.State,
    email: TextField.State,
    bio: TextField.State,
    fm: FocusManager,
    submitted: Option[Submission],
    darkTheme: Boolean
  )

  import Msg.*

  /** Global key bindings. Editing keys for the focused field/button are routed separately. */
  val Keys: Keymap[Msg] =
    Keymap.quit(Quit) ++
      Keymap.focus(next = NextFocus, previous = PrevFocus) ++
      Keymap(
        KeyDecoder.InputKey.CharKey('t') -> ToggleTheme,
        KeyDecoder.InputKey.CharKey('T') -> ToggleTheme
      )

  /** Initial state for the three fields — placeholders shown until typed-in. */
  private def freshFields: (TextField.State, TextField.State, TextField.State) =
    (
      TextField.State.withPlaceholder("Alice"),
      TextField.State.withPlaceholder("alice@example.com"),
      TextField.State.withPlaceholder("(optional bio)")
    )

  object App extends TuiApp[Model, Msg]:

    private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Sub.InputKey(ConsoleInputKey.apply, ConsoleInputError.apply, ctx)
      val (n, e, b) = freshFields
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        name = n,
        email = e,
        bio = b,
        fm = FocusManager(FocusOrder),
        submitted = None,
        darkTheme = true
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncTerminalSize(m, ctx)
      msg match
        case NextFocus =>
          sized.copy(fm = sized.fm.next).tui

        case PrevFocus =>
          sized.copy(fm = sized.fm.previous).tui

        case ToggleTheme =>
          sized.copy(darkTheme = !sized.darkTheme).tui

        case Quit =>
          Tui(sized, Cmd.Exit)

        case Activate =>
          sized.fm.current match
            case Some(id) if id == SubmitId =>
              sized
                .copy(
                  submitted = Some(Submission(sized.name.buffer, sized.email.buffer, sized.bio.buffer))
                )
                .tui
            case Some(id) if id == ResetId =>
              update(sized, Reset, ctx)
            case _ => sized.tui

        case Reset =>
          val (n, e, b) = freshFields
          sized.copy(name = n, email = e, bio = b, submitted = None).tui

        case ConsoleInputKey(key) =>
          // Global bindings always win.
          Keys.lookup(key) match
            case Some(next) => update(sized, next, ctx)
            case None       =>
              // Route uncaught keys to whichever element is focused. Each
              // TextField uses Enter to advance focus (keeping its buffer);
              // each Button activates on Enter / Space.
              sized.fm.current match
                case Some(id) if id == NameId =>
                  routeField(sized, key, _.name, (m, ns) => m.copy(name = ns), ctx)
                case Some(id) if id == EmailId =>
                  routeField(sized, key, _.email, (m, ns) => m.copy(email = ns), ctx)
                case Some(id) if id == BioId =>
                  routeField(sized, key, _.bio, (m, ns) => m.copy(bio = ns), ctx)
                case Some(id) if id == SubmitId || id == ResetId =>
                  key match
                    case KeyDecoder.InputKey.Enter | KeyDecoder.InputKey.CharKey(' ') =>
                      update(sized, Activate, ctx)
                    case _ => sized.tui
                case _ => sized.tui

        case ConsoleInputError(_) => sized.tui

    /**
     * Forward a key to a focused [[TextField]] and pump any resulting message
     * (typically `NextFocus` on Enter) back through `update`.
     */
    private def routeField(
      m: Model,
      key: KeyDecoder.InputKey,
      get: Model => TextField.State,
      set: (Model, TextField.State) => Model,
      ctx: RuntimeCtx[Msg]
    ): Tui[Model, Msg] =
      val (next, maybeMsg) = TextField.handleKey(get(m), key)(_ => Some(NextFocus))
      val nextModel        = set(m, next)
      maybeMsg match
        case Some(follow) => update(nextModel, follow, ctx)
        case None         => nextModel.tui

    override def view(m: Model): RootNode =
      given Theme = if m.darkTheme then Theme.dark else Theme.light
      val theme   = summon[Theme]

      val termWidth  = math.max(60, m.terminalWidth)
      val termHeight = math.max(20, m.terminalHeight)
      val fieldWidth = 30

      def fieldRow(label: String, state: TextField.State, focused: Boolean): Layout =
        Layout.row(gap = 2)(
          TextNode(1.x, 1.y, List(Text(label.padTo(8, ' '), Style(fg = theme.foreground)))),
          TextField.view(state, lineWidth = fieldWidth, focused = focused)
        )

      val title = Layout.Elem(
        TextNode(
          1.x,
          1.y,
          List(Text("User Profile Form", Style(fg = theme.primary, bold = true, underline = true)))
        )
      )

      val nameRow  = fieldRow("Name:", m.name, m.fm.isFocused(NameId))
      val emailRow = fieldRow("Email:", m.email, m.fm.isFocused(EmailId))
      val bioRow   = fieldRow("Bio:", m.bio, m.fm.isFocused(BioId))

      val buttons = Layout.row(gap = 2)(
        Button("Submit", focused = m.fm.isFocused(SubmitId)),
        Button("Reset", focused = m.fm.isFocused(ResetId))
      )

      val submittedLine: Layout = m.submitted match
        case None =>
          Layout.Elem(
            TextNode(1.x, 1.y, List(Text("(no submission yet)", Style(fg = theme.foreground))))
          )
        case Some(s) =>
          Layout.Elem(
            TextNode(
              1.x,
              1.y,
              List(
                Text("Last submitted: ", Style(fg = theme.success, bold = true)),
                Text(s"name=${displayOrPlaceholder(s.name, m.name.placeholder)}", Style(fg = theme.foreground)),
                Text(", ", Style(fg = theme.foreground)),
                Text(s"email=${displayOrPlaceholder(s.email, m.email.placeholder)}", Style(fg = theme.foreground))
              )
            )
          )

      val help = Layout.Elem(
        TextNode(
          1.x,
          1.y,
          List(
            Text(
              "Tab: next   Enter: next/activate   t: theme   q: quit",
              Style(fg = theme.foreground)
            )
          )
        )
      )

      val column = Layout.Column(
        gap = 1,
        children = List(
          title,
          Layout.Spacer(1, 1),
          nameRow,
          emailRow,
          bioRow,
          Layout.Spacer(1, 1),
          buttons,
          Layout.Spacer(1, 1),
          submittedLine,
          Layout.Spacer(1, 1),
          help
        )
      )

      RootNode(
        width = termWidth,
        height = termHeight,
        children = column.resolve(Coord(2.x, 2.y)),
        input = None
      )

    private def displayOrPlaceholder(value: String, placeholder: String): String =
      if value.isEmpty then s"<$placeholder>" else value

    override def toMsg(input: PromptLine): Result[Msg] =
      // No prompt is registered; this is unreachable in normal operation.
      Left(TermFlowError.CommandError(input.value))
