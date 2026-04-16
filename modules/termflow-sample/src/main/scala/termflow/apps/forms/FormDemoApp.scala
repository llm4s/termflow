package termflow.apps.forms

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets.*

/**
 * Multi-field form demo wiring [[TextField]], [[FocusManager]], [[Keymap]],
 * and [[Layout]] together. Three editable fields (Name / Email / Bio) plus
 * Submit and Reset buttons share one focus order; Tab cycles forward,
 * Shift+Tab backward, and Enter does the right thing per element type.
 *
 * ## Keys
 *
 *   - `Tab` / `Shift+Tab`  cycle focus forward / backward (Name → Email → Bio → Submit → Reset)
 *   - `↑` / `↓`            same as Shift+Tab / Tab — work anywhere, including inside a text field
 *   - `←` / `→`            on a button, move to the previous / next focus;
 *                          inside a text field, move the in-field cursor (does NOT change focus)
 *   - `Enter` (in field)   submit the form (capture all field values)
 *   - `Enter` / `Space` (button) activate Submit / Reset
 *   - `Backspace` / `Home` / `End` / `Delete` — standard text editing in the focused field
 *   - `Ctrl+T`             toggle dark / light theme (works even inside a text field)
 *   - `t` (when not in a field) also toggles theme
 *   - `q` (when not in a field) / `Ctrl+C` / `Esc` quit
 *
 * `t` / `q` and `←` / `→` are deliberately **not** in the true-global keymap —
 * they would collide with legitimate text input. Inside a focused
 * [[TextField]] those keys are routed straight to the field; on a focused
 * [[Button]] (or with no focus) they fall through to the non-text shortcuts
 * layer.
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

  /** `true` when `id` corresponds to one of the editable text fields. */
  def isFieldId(id: FocusId): Boolean = id == NameId || id == EmailId || id == BioId

  /** Form result captured when Submit is activated (or Enter fires from a field). */
  final case class Submission(name: String, email: String, bio: String)

  enum Msg:
    case NextFocus
    case PrevFocus
    case Submit
    case Reset
    case ToggleTheme
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

  /**
   * Keys that fire regardless of which element is focused. These never
   * conflict with text input because they're either control sequences
   * (`Ctrl+T`, `Ctrl+C`, `Esc`), special keys (`Tab`, `Shift+Tab`), or
   * vertical arrows (TextField is single-line so it doesn't consume them).
   */
  val Globals: Keymap[Msg] =
    Keymap(
      KeyDecoder.InputKey.Ctrl('C') -> Quit,
      KeyDecoder.InputKey.Escape    -> Quit,
      KeyDecoder.InputKey.Ctrl('T') -> ToggleTheme
    ) ++
      Keymap.focus(next = NextFocus, previous = PrevFocus) ++
      Keymap.focusVertical(previous = PrevFocus, next = NextFocus)

  /**
   * Shortcuts that only fire when focus is **not** on a [[TextField]] —
   * inside a field they would collide with legitimate editing keys:
   *   - `ArrowLeft` / `ArrowRight` move the in-field cursor there
   *   - letter keys insert into the buffer
   *
   * Buttons (and no-focus) fall through to this layer after [[Globals]].
   */
  val NonTextShortcuts: Keymap[Msg] =
    Keymap(
      KeyDecoder.InputKey.CharKey('t') -> ToggleTheme,
      KeyDecoder.InputKey.CharKey('T') -> ToggleTheme,
      KeyDecoder.InputKey.CharKey('q') -> Quit,
      KeyDecoder.InputKey.CharKey('Q') -> Quit
    ) ++
      Keymap.focusHorizontal(previous = PrevFocus, next = NextFocus)

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
        case NextFocus   => sized.copy(fm = sized.fm.next).tui
        case PrevFocus   => sized.copy(fm = sized.fm.previous).tui
        case ToggleTheme => sized.copy(darkTheme = !sized.darkTheme).tui
        case Quit        => Tui(sized, Cmd.Exit)

        case Submit =>
          sized
            .copy(submitted = Some(Submission(sized.name.buffer, sized.email.buffer, sized.bio.buffer)))
            .tui

        case Reset =>
          val (n, e, b) = freshFields
          sized.copy(name = n, email = e, bio = b, submitted = None).tui

        case ConsoleInputKey(key) =>
          // 1. True globals (Tab / Shift+Tab / Ctrl+C / Esc / Ctrl+T) win
          //    everywhere — including inside a TextField — because they
          //    don't collide with any printable input.
          Globals.lookup(key) match
            case Some(next) => update(sized, next, ctx)
            case None       =>
              // 2. Otherwise route by focused element. Fields get all
              //    remaining keys; buttons activate on Enter/Space and
              //    fall through to NonTextShortcuts for letter shortcuts.
              sized.fm.current match
                case Some(id) if isFieldId(id) =>
                  routeField(sized, id, key, ctx)
                case Some(id) if id == SubmitId =>
                  key match
                    case KeyDecoder.InputKey.Enter | KeyDecoder.InputKey.CharKey(' ') =>
                      update(sized, Submit, ctx)
                    case _ =>
                      NonTextShortcuts.lookup(key).fold(sized.tui)(m => update(sized, m, ctx))
                case Some(id) if id == ResetId =>
                  key match
                    case KeyDecoder.InputKey.Enter | KeyDecoder.InputKey.CharKey(' ') =>
                      update(sized, Reset, ctx)
                    case _ =>
                      NonTextShortcuts.lookup(key).fold(sized.tui)(m => update(sized, m, ctx))
                case _ =>
                  NonTextShortcuts.lookup(key).fold(sized.tui)(m => update(sized, m, ctx))

        case ConsoleInputError(_) => sized.tui

    /**
     * Forward a key to a focused [[TextField]]. Enter triggers a form
     * Submit via [[TextField.handleKeyOrSubmit]]; every other key edits
     * the field buffer.
     */
    private def routeField(
      m: Model,
      id: FocusId,
      key: KeyDecoder.InputKey,
      ctx: RuntimeCtx[Msg]
    ): Tui[Model, Msg] =
      def setField(state: TextField.State): Model =
        if id == NameId then m.copy(name = state)
        else if id == EmailId then m.copy(email = state)
        else m.copy(bio = state)
      def getField: TextField.State =
        if id == NameId then m.name
        else if id == EmailId then m.email
        else m.bio

      val (next, maybeMsg) = TextField.handleKeyOrSubmit(getField, key)(Submit)
      val nextModel        = setField(next)
      maybeMsg match
        case Some(follow) => update(nextModel, follow, ctx)
        case None         => nextModel.tui

    override def view(m: Model): RootNode =
      given Theme = if m.darkTheme then Theme.dark else Theme.light
      val theme   = summon[Theme]

      val termWidth  = math.max(60, m.terminalWidth)
      val termHeight = math.max(20, m.terminalHeight)
      val fieldWidth = 30
      val themeName  = if m.darkTheme then "dark" else "light"

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
                Text(
                  s"name=${displayOrPlaceholder(s.name, m.name.placeholder)}, " +
                    s"email=${displayOrPlaceholder(s.email, m.email.placeholder)}",
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
          submittedLine
        )
      )

      // Inverse-video status bar pinned to the bottom row. This is the only
      // row guaranteed to be readable across all terminal backgrounds — the
      // rest of the demo uses theme.foreground which can collide with the
      // user's terminal bg colour (see the Theme ScalaDoc note about
      // background slots). When toggling themes, watch the inverse colours
      // here flip — that's the always-visible signal the toggle worked.
      val helpBar = StatusBar(
        left = " form ",
        center = "Tab/⇧Tab/↑↓: nav  Enter: submit  Ctrl+T: theme  Ctrl+C: quit",
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

    private def displayOrPlaceholder(value: String, placeholder: String): String =
      if value.isEmpty then s"<$placeholder>" else value

    override def toMsg(input: PromptLine): Result[Msg] =
      // No prompt is registered; this is unreachable in normal operation.
      Left(TermFlowError.CommandError(input.value))
