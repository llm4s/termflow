# Show a confirm dialog and act on the answer

`Dialogs.confirm` returns an `Overlay` value — it does *not* take
callbacks. The app owns the dialog's open/closed state and the
focused button; key events route to whichever side owns focus.

## Pattern

1. Add a flag (or sealed `case class`) to your model that says "the
   confirm dialog is open and waiting for an answer".
2. In `view`, when that flag is set, append the `Dialogs.confirm`
   overlay to `RootNode.overlays`.
3. In `update`, route key events to the dialog when it's open: arrow
   keys flip the focused button, `Enter`/`Space` commits, `Escape`
   cancels.

## Code

```scala
import termflow.tui.{Dialogs, Theme}
import termflow.tui.KeyDecoder.InputKey.*

final case class Model(
  /* … domain fields … */,
  confirm: Option[ConfirmState]
)

enum ConfirmState:
  case Open(yesFocused: Boolean)   // dialog visible, button focus tracked here

enum Msg:
  case AskDelete             // user requested the destructive action
  case ConfirmYes
  case ConfirmNo
  case ConfirmKey(k: InputKey)

def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Msg.AskDelete =>
      m.copy(confirm = Some(ConfirmState.Open(yesFocused = false))).tui

    case Msg.ConfirmKey(k) =>
      m.confirm match
        case Some(ConfirmState.Open(yesFocused)) =>
          k match
            case ArrowLeft | ArrowRight | Tab | BackTab =>
              m.copy(confirm = Some(ConfirmState.Open(!yesFocused))).tui
            case Enter | CharKey(' ') =>
              if yesFocused then m.copy(confirm = None).gCmd(Msg.ConfirmYes)
              else              m.copy(confirm = None).gCmd(Msg.ConfirmNo)
            case Escape =>
              m.copy(confirm = None).gCmd(Msg.ConfirmNo)
            case _ => m.tui
        case None => m.tui

    case Msg.ConfirmYes =>  /* perform the destructive action */
    case Msg.ConfirmNo  =>  m.tui /* no-op */

def view(m: Model): RootNode =
  given Theme = Theme.dark
  val baseChildren = /* … */
  val overlays = m.confirm match
    case Some(ConfirmState.Open(yesFocused)) =>
      List(Dialogs.confirm(
        prompt     = "Delete this file?",
        yesFocused = yesFocused,
        title      = "Confirm delete",
        yesLabel   = "Yes",
        noLabel    = "No"
      ))
    case None => Nil

  RootNode(80, 24, children = baseChildren, input = None, overlays = overlays)
```

## Notes

- **Why `Option[ConfirmState]` and not just `Boolean`?** It scales —
  add `Open(yesFocused, prompt: String)` later and any caller of
  `AskDelete` can pass its own prompt.
- **Always re-route keys when a dialog is open.** Don't forget to
  intercept the global keymap so `q` doesn't quit while the dialog is
  modal. The simplest pattern is: if `m.confirm.isDefined`, route the
  whole keystream into `ConfirmKey` and skip your normal handlers.
- **Mouse clicks on overlays.** `Dialogs.confirm` is mouse-aware in
  the renderer; if you want clicks on the buttons to flip focus,
  pair the dialog with `Layout.resolveTracked` and dispatch
  `ConfirmKey` from the hit-test result.

For a working example, see the showcase's *Dialogs* tab —
[`Stage1ShowcaseApp.scala`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/showcase/Stage1ShowcaseApp.scala).
