# Architecture

TermFlow is split into four published modules, layered so each one is
publicly usable without forcing you to climb the whole stack:

| Module | What it gives you | Key types |
|---|---|---|
| `termflow-terminal` | Raw access to the underlying TTY: backend, key decoding, capability detection. | `TerminalBackend`, `KeyDecoder`, `Capabilities`, `Grapheme`, `WCWidth` |
| `termflow-screen` | A character grid you draw into, plus diff-rendering to ANSI. | `RenderFrame`, `AnsiRenderer`, `Layout`, `HitTest` |
| `termflow-app` | The Elm-style runtime: `TuiApp`, `Cmd`, `Sub`, `FocusManager`, `Dialogs`. | `TuiApp`, `TuiRuntime`, `Cmd`, `Sub`, `Tui` |
| `termflow-widgets` | Reusable components built on top of the app layer. | `Button`, `TextField`, `ListView`, `Table`, `Tree`, `Tabs`, `Form`, ... |

Plus an umbrella `termflow` artefact that depends on all four, and a
`termflow-testkit` artefact for testing apps without a real terminal.

## The runtime loop

`TuiRuntime.run(app)` drives the loop:

```text
                  ┌─────────────────────┐
                  │  TuiApp[Model, Msg] │
                  │   init / update /   │
                  │     view / toMsg    │
                  └─────────────────────┘
                           ▲   │
                       Msg │   │ Cmd / Tui
                           │   ▼
                  ┌─────────────────────┐
       Sub.* ────▶│      CmdBus         │
   (Sub.Every,    │  (blocking queue)   │
   Sub.InputKey,  └─────────────────────┘
   Sub.Resize)              │
                            ▼
                  ┌─────────────────────┐
                  │     AnsiRenderer    │
                  │   (frame diffing)   │
                  └─────────────────────┘
                            │
                            ▼
                          ANSI
```

1. **`init`** produces an initial `Tui[Model, Msg]` — the starting model
   plus an optional startup `Cmd` (typically registering subscriptions).
2. **Subscriptions** push `Cmd`s onto the `CmdBus`. Examples: every key
   press becomes a `Cmd.GCmd(msg)`, every timer tick the same.
3. The runtime drains the bus, dispatches each `Msg` to **`update`**, and
   gets back a new `Tui[Model, Msg]` — the next model and an optional
   follow-up command.
4. **`view`** renders the new model into a `RootNode` virtual DOM.
5. **`AnsiRenderer`** diffs the new `RootNode` against the previous frame
   and emits the minimum ANSI escape sequence to bring the terminal up to
   date.

Loop, repeat.

## What runs where

- **Update is synchronous and pure.** No I/O. If you need async work,
  return a `Cmd.FCmd[Future[A], Msg]` and the runtime will run the
  `Future`, deliver the result back as a `Msg`.
- **View is pure.** It returns a `RootNode` from a `Model`. The renderer
  takes care of cursor placement, colour emission, and clipping.
- **Subscriptions live outside `update`.** `Sub.InputKey` reads from JLine
  on a background thread; `Sub.Every` schedules ticks. Both push `Cmd`s
  onto the bus so `update` only ever sees `Msg`s.

## Why three layers?

The split lets you opt out of higher layers when they get in the way:

- Building a one-shot CLI utility that just needs raw key reads and
  capability detection? Depend on `termflow-terminal` only.
- Building a custom layout engine on top of a screen buffer? Depend on
  `termflow-screen`.
- Building a typical interactive TUI? Depend on the umbrella `termflow`
  artefact, which pulls in all four.

The boundaries also mean each module can carry its own MiMa baseline once
1.0 ships, so a breaking change in `widgets` doesn't force a major bump
on the whole stack.

For deeper architectural rationale, see the
[contributor design doc](../contrib/DESIGN.md) and the
[render pipeline doc](../contrib/RENDER_PIPELINE.md).

Next up: [Install](install.md).
