# Clean shutdown on `Ctrl-C` and on resize

Two events that *should* always restore the terminal cleanly:

1. **`Ctrl-C`** — user wants to quit, fast.
2. **Window resize** — not a shutdown event itself, but a frequent
   trigger for "my app crashed and now my terminal is broken".

The runtime handles most of this for you. The remaining work is
making sure your app dispatches `Cmd.Exit` from the right places.

## What the runtime already does

- `TuiRuntime.run` registers a JVM shutdown hook that restores the
  terminal (alt-buffer off, cursor visible, raw mode off, mouse
  tracking off) even on uncaught exceptions or `kill`.
- All `Sub`s registered via `RuntimeCtx` (`Sub.Every`, `Sub.InputKey`,
  `Sub.TerminalResize`) auto-cancel on `Cmd.Exit` *and* on shutdown.
- The renderer watches for SIGWINCH (size change) and re-renders
  without you doing anything.

## What you have to wire

- A `Quit` `Msg` and a `Cmd.Exit` follow-up for it.
- A keystroke that produces `Quit` for `Ctrl-C` (and ideally `q`).
- An optional re-read of `ctx.terminal.{width, height}` on every
  `update` so the model knows the current size.

## Code: the canonical wiring

```scala
import termflow.tui.{Cmd, KeyDecoder, RuntimeCtx, Sub, Tui, TuiApp, TuiRuntime, RootNode}
import termflow.tui.KeyDecoder.InputKey.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

object MyApp:

  final case class Model(
    width:  Int,
    height: Int,
    input:  Sub[Msg]
  )

  enum Msg:
    case KeyPressed(k: KeyDecoder.InputKey)
    case Quit

  object App extends TuiApp[Model, Msg]:

    private def syncSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.width && h == m.height then m else m.copy(width = w, height = h)

    def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val keys = Sub.InputKey[Msg](
        msg     = Msg.KeyPressed.apply,
        onError = _ => Msg.Quit,                  // hard-quit on input pump errors
        ctx     = ctx
      )
      Model(ctx.terminal.width, ctx.terminal.height, keys).tui

    def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncSize(m, ctx)
      msg match
        case Msg.KeyPressed(Ctrl('C'))  => Tui(sized, Cmd.Exit)
        case Msg.KeyPressed(CharKey('q')) => Tui(sized, Cmd.Exit)
        case Msg.KeyPressed(_)          => sized.tui
        case Msg.Quit                   => Tui(sized, Cmd.Exit)

    def view(m: Model): RootNode =
      RootNode(m.width, m.height, children = /* … */, input = None)

    def toMsg(input: PromptLine): Result[Msg] = Right(Msg.Quit)

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)
```

## Notes

- **`syncSize` on every update** is the cheapest way to stay correct
  on resize. The cost is a single `==` comparison per message —
  effectively free.
- **`Ctrl-C` should always quit.** Don't be tempted to swallow it:
  if your app hangs, the user will reach for `Ctrl-C` and expect a
  graceful exit.
- **Quit during a modal dialog.** If you have an open `Dialogs`
  overlay, `Ctrl-C` can either close the dialog or quit the whole
  app. Pick one and be consistent. Most apps treat `Ctrl-C` as
  unconditional quit and `Esc` as "close the dialog".
- **Quit during text input.** Inside a `Prompt` or `TextField`,
  `q` is a literal letter — only quit on `q` *outside* text fields.
  The wizard sample shows the
  [quit-when-not-in-text-field](../tut/04-forms-and-dialogs.md#7-quit-semantics--quit-when-not-in-text-field)
  idiom.
- **Don't `System.exit`.** It bypasses the runtime's cleanup. Always
  return `Cmd.Exit` from `update`; the runtime takes it from there.
- **Crash recovery.** If your app crashes despite all of this,
  `reset` in your shell will get the terminal back. The shutdown
  hook makes this rare.

For a working example, every demo app under
[`modules/termflow-sample/.../apps`](https://github.com/llm4s/termflow/tree/main/modules/termflow-sample/src/main/scala/termflow/apps)
follows this pattern.
