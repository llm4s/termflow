package termflow.apps.dialog

import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

/**
 * Demonstrates the modal-overlay primitive shipped with #153.
 *
 * The base view is a counter. Pressing `d` opens a confirm dialog asking
 * whether to reset the counter. While the dialog is up:
 *   - all counter key bindings are ignored (modal capture)
 *   - the dialog's left/right arrows move focus between Yes / No
 *   - Enter activates the focused choice; Esc cancels
 *
 * Press `q` (with no dialog) to quit.
 */
object DialogDemoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  private given Theme = Theme.rounded

  enum Dialog:
    case None
    case ConfirmReset(yesFocused: Boolean)

  final case class Model(count: Int, dialog: Dialog, input: Sub[Msg])

  enum Msg:
    case Inc
    case Dec
    case OpenReset
    case CloseDialog
    case ToggleDialogFocus
    case ConfirmYes
    case ConfirmNo
    case Quit
    case Key(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        count = 0,
        dialog = Dialog.None,
        input = Sub.InputKey(k => Key(k), e => KeyError(e), ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Inc         => m.copy(count = m.count + 1).tui
        case Dec         => m.copy(count = m.count - 1).tui
        case OpenReset   => m.copy(dialog = Dialog.ConfirmReset(yesFocused = false)).tui
        case CloseDialog => m.copy(dialog = Dialog.None).tui
        case ToggleDialogFocus =>
          m.dialog match
            case Dialog.ConfirmReset(yes) => m.copy(dialog = Dialog.ConfirmReset(!yes)).tui
            case Dialog.None              => m.tui
        case ConfirmYes  => m.copy(count = 0, dialog = Dialog.None).tui
        case ConfirmNo   => m.copy(dialog = Dialog.None).tui
        case Quit        => Tui(m, Cmd.Exit)
        case KeyError(_) => m.tui
        case Key(k)      => Tui(m, dispatch(m, k))

    /**
     * Key dispatch is gated on whether a dialog is open. This is the
     * point of the modal: while a dialog is up, the underlying app does
     * not respond to its own bindings.
     */
    private def dispatch(m: Model, k: KeyDecoder.InputKey): Cmd[Msg] =
      import KeyDecoder.InputKey.*
      m.dialog match
        case Dialog.ConfirmReset(yesFocused) =>
          k match
            case ArrowLeft | ArrowRight => Cmd.GCmd(ToggleDialogFocus)
            case Enter                  => if yesFocused then Cmd.GCmd(ConfirmYes) else Cmd.GCmd(ConfirmNo)
            case Escape                 => Cmd.GCmd(CloseDialog)
            case _                      => Cmd.NoCmd
        case Dialog.None =>
          k match
            case CharKey('+') | ArrowUp                     => Cmd.GCmd(Inc)
            case CharKey('-') | ArrowDown                   => Cmd.GCmd(Dec)
            case CharKey('d') | CharKey('D') | CharKey('r') => Cmd.GCmd(OpenReset)
            case CharKey('q') | CharKey('Q') | Escape       => Cmd.GCmd(Quit)
            case _                                          => Cmd.NoCmd

    override def view(m: Model): RootNode =
      val title   = TextNode(2.x, 1.y, List("Dialog Demo".themed(_.primary)))
      val counter = TextNode(2.x, 3.y, List("Count: ".text, m.count.toString.themed(_.success)))
      val help1   = TextNode(2.x, 5.y, List("+ / ↑ : increment    - / ↓ : decrement".text))
      val help2   = TextNode(2.x, 6.y, List("d : open reset dialog    q / Esc : quit".text))
      val baseRoot = RootNode(
        width = 60,
        height = 12,
        children = List(title, counter, help1, help2),
        input = None
      )

      m.dialog match
        case Dialog.None => baseRoot
        case Dialog.ConfirmReset(yesFocused) =>
          baseRoot.copy(overlays =
            List(
              Dialogs.confirm(
                prompt = s"Reset counter (currently ${m.count}) to zero?",
                yesFocused = yesFocused
              )
            )
          )

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation("DialogDemo has no prompt"))
