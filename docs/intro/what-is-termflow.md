# What is TermFlow?

TermFlow is a small, functional terminal UI framework for Scala 3. You
describe your app as three pure functions — `init`, `update`, and `view` —
and TermFlow turns that description into a fully-rendered, mouse-aware,
diff-painted ANSI terminal program.

```text
┌──────────────┐  Msg   ┌──────────┐ Model  ┌────────┐
│   Sub.*      │───────▶│  update  │───────▶│  view  │──▶ ANSI
│ (input/time) │        └──────────┘        └────────┘
└──────────────┘             │                  ▲
       ▲                     │ Cmd              │
       └─── async / FCmd ◀───┘                  │
                            (next frame) ───────┘
```

## When TermFlow is a good fit

- **Prompt-driven CLIs.** Anything where you'd reach for `readline` plus
  fancy output. TermFlow's `Prompt` widget gives you a real input field
  with cursor, history, paste support, and grapheme-aware editing.
- **Streaming output.** LLM-token streaming, log tails, build output.
  `Cmd.FCmd` lets you fire async work and merge the results into the model.
- **Dashboards.** Multi-pane layouts with live counters, progress bars,
  spinners, and a status bar — `Sub.Every` drives the heartbeat.
- **Multi-step flows.** Wizards, file pickers, action menus.
  `FocusManager` plus the `Dialogs` helpers handle most form patterns
  out-of-the-box.

## When to reach for something else

- **You need plain `printf` output.** TermFlow takes over the terminal —
  it switches to the alternate buffer, hides the cursor, raw-mode-locks
  input. For "print-and-exit" tools just use `println`.
- **You need a real GUI.** TermFlow's a TUI; it can't render images, only
  cells. If you need pixels, use Swing, JavaFX, or Compose.
- **You need to support a Scala 2.13 runtime.** Use the `legacy-213-track`
  branch instead — main is Scala 3 only.

## Compared with other libraries

TermFlow's three-layer model (`terminal` ↔ `screen` ↔ `app+widgets`)
mirrors [Lanterna](https://github.com/mabe02/lanterna) more closely than
[bubbletea](https://github.com/charmbracelet/bubbletea) — every layer is
publicly usable on its own. The Elm-style `update` / `view` shape comes
from bubbletea and from Scala-side experiments like
[indigo](https://indigoengine.io/). The result is a library that you can
drop down into when you need raw terminal access, or use at the top level
for a fully-managed TUI.

Next up: [Architecture](architecture.md) walks the layers in detail.
