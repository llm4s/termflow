# Flag a session as needing attention

Long-running TUIs often want to ping the user when something interesting
happens — a build finished, a streaming reply landed, an alert fired. The
classic UX is the **terminal bell** (which most modern terminals translate
into a tab-bar activity indicator) plus, where supported, a real **desktop
notification** with a title and body.

TermFlow exposes two `Cmd` variants for this:

- **`Cmd.RequestAttention`** — minimum signal. Emits BEL on every backend
  except `Disabled`; iTerm2 also bounces the dock icon via
  `OSC 1337 RequestAttention`.
- **`Cmd.Notify(title, body)`** — desktop notification when the terminal
  has a protocol for it; falls back to BEL otherwise.

## Quick example

```scala
import termflow.tui.*
import termflow.tui.Tui.*

enum Msg:
  case StreamFinished(reply: String)
  case BackgroundJobFailed(err: String)
  case Quit

def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Msg.StreamFinished(reply) =>
      Tui(
        model.copy(reply = Some(reply)),
        Cmd.Notify("Reply ready", reply.take(60))
      )

    case Msg.BackgroundJobFailed(err) =>
      // BEL + iTerm2 RequestAttention. Cheaper than a full notification —
      // useful when the user is more likely to be watching the tab bar
      // than a notification centre.
      Tui(model.copy(error = Some(err)), Cmd.RequestAttention)

    case Msg.Quit => Tui(model, Cmd.Exit)
```

That's it. The runtime delegates to `TerminalBackend.notify` /
`TerminalBackend.requestAttention`, which select the right escape sequence
from the detected `Capabilities.notifications`.

## What gets emitted

| Terminal | Detection | Sequence |
|---|---|---|
| iTerm2 | `TERM_PROGRAM=iTerm.app` | `OSC 9 ; <message> BEL` and `OSC 1337 ; RequestAttention=yes BEL` |
| kitty | `KITTY_WINDOW_ID` set or `TERM=xterm-kitty` | `OSC 99 ; <metadata> ; <body> ST` |
| GNOME Terminal / Tilix / xfce4-terminal | `VTE_VERSION` set | `OSC 777 ; notify ; <title> ; <body> BEL` |
| Anything else (xterm, Alacritty, WezTerm, …) | TERM looks like a real terminal | `BEL` (`\x07`) |
| `dumb` / unset `TERM` | — | nothing (`Disabled`) |

The current detection lives in
[`Capabilities.detect`](https://github.com/llm4s/termflow/blob/main/modules/termflow-terminal/src/main/scala/termflow/tui/Capabilities.scala);
override the result by constructing `Capabilities` directly, or set
`TERMFLOW_NOTIFICATIONS=off|bell|auto` to force a kind:

- `off` — never emit anything.
- `bell` — always emit just BEL, even on iTerm2 / kitty / VTE.
- `auto` (default) — sniff the environment, pick the richest available kind.

## Inside tmux / screen

`tmux` and `screen` translate BEL into an in-status-bar window-bell or
activity flag (subject to `set -g monitor-bell on`). That covers the
"flag this pane needs attention" use case directly — no extra wiring
required.

The richer OSC sequences (`OSC 9`, `OSC 99`, `OSC 777`) **do not** reach the
outer terminal by default. tmux can be configured to forward them via DCS
passthrough (`set -g allow-passthrough on`) but this is off by default and
TermFlow does not currently auto-wrap. If you need desktop notifications
from inside tmux, either enable passthrough on the target session or rely
on the BEL fallback.

## Testing notifications

`termflow-testkit`'s `TuiTestDriver` records every `Cmd.RequestAttention`
and `Cmd.Notify` so apps can assert on them without a real terminal:

```scala
val driver = TuiTestDriver(MyApp, width = 80, height = 24)
driver.init()
driver.send(Msg.StreamFinished("hello"))

assert(driver.observedNotifications == List("Reply ready" -> "hello"))
assert(driver.attentionCount == 0)
```

No real escape bytes leave the harness — the driver intercepts the
commands at dispatch time.

## When to use which

- **Reach for `Cmd.RequestAttention`** for cheap, frequent signals: a job
  failed, an alert fired, an error came back from a streaming call. Most
  terminals will silently fold a flurry of BELs into one activity flag.
- **Reach for `Cmd.Notify`** for events the user genuinely wants to read
  in a notification centre: a build finished after several minutes, an
  agent finished a long task, a pull request is ready for review. Most OSes
  rate-limit notifications, so aggressive use will get the user to silence
  the app.

## Caveats

- **Headless / SSH.** BEL still works (it is just a byte). Desktop
  notifications reach whichever terminal the user is sitting at — the
  remote host has no idea whether the local terminal renders them.
- **No newlines / control bytes in titles or bodies.** TermFlow strips
  C0 controls before emitting, so unsanitised user input will not break
  the OSC envelope.
- **Always opt in.** Don't fire on every keystroke or every frame. Tie
  notifications to model transitions the user actually cares about.
