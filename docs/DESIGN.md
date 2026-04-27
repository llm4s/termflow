# TermFlow Design

TermFlow is a small terminal UI (TUI) framework for Scala built around a simple,
functional architecture.

This document captures the current design and the intended direction.

## Core Architecture

TermFlow apps follow a familiar “model / update / view” structure:

- **Model**: your application state (plain case classes)
- **Update**: a function that takes `(model, msg)` and returns a new model plus an optional command
- **View**: a function that takes the model and returns a render tree (a small virtual DOM)

### `Tui`

The main return type from `update` is:

- `Tui[Model, Msg]` which contains:
  - `model: Model`
  - `cmd: Cmd[Msg]`

### `Cmd` (commands)

Commands represent side effects that happen “after” an update:

- `Cmd.GCmd(msg)` – enqueue a message for the next update loop
- `Cmd.FCmd(...)` – run an async task and publish its result as a message
- `Cmd.asyncResult(task, onSuccess, onError)` – idiomatic helper for the
  common case where the async task returns `AsyncResult[A]` (i.e.
  `Future[Result[A]]`). Routes successes through `onSuccess`, logical
  failures through `onError`, and lets the runtime surface infrastructure
  failures via `Cmd.TermFlowErrorCmd` automatically.
- `Cmd.Exit` – exit the runtime
- `Cmd.TermFlowErrorCmd(err)` – surface a recoverable error to the renderer

This keeps `update` mostly pure and makes long-running work explicit.

**Effect-system stance.** TermFlow uses `scala.concurrent.Future` and the
framework's `Result[A] = Either[TermFlowError, A]`, exposed as
`type AsyncResult[+A] = Future[Result[A]]` in `TuiPrelude`. The alias
mirrors the `llm4s` core 1:1 so values pass between the two libraries
without an adapter. We do not ship cats-effect or ZIO integration modules;
apps that use those bridge to a `Future` at the `Cmd` boundary.

### `Sub` (subscriptions)

Subscriptions are event sources that publish messages over time:

- input keypress stream
- timers (e.g., spinners, uptime tick)

Subscriptions can be started/stopped by holding them in the model and calling
`cancel()` when they’re no longer needed.

### `Prompt`

Prompt handling is integrated as a reusable helper:

- it consumes normalized `InputKey`
- maintains a buffer + cursor
- provides `render` output and a cursor position
- supports command-like flows where pressing Enter yields a `Msg` or validation error

## Rendering

TermFlow renders a small virtual DOM consisting of nodes like:

- `TextNode`, `InputNode`, `BoxNode`

The runtime uses an ANSI renderer with:

- command coalescing in the runtime loop
- framebuffer materialization
- frame-to-frame diffing that emits minimal ANSI patches

See [`RENDER_PIPELINE.md`](./RENDER_PIPELINE.md) for render flow, invariants, and regression focus areas.

## Testing

Most logic should be testable by:

- unit-testing `update` functions (pure state transitions)
- unit-testing prompt editing behavior (`PromptSpec`)
- unit-testing history/search behavior (`HistorySpec`)

The runtime loop is intentionally small; most behavior should live in update/view helpers.
