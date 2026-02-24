# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Compile (Scala 3)
sbt compile

# Run tests
sbt test

# Format code
sbt scalafmtAll

# Check formatting (CI)
sbt scalafmtCheckAll

# Publish locally
sbt publishLocal
```

## Running Sample Apps

```bash
# Main demo
sbt "termflowSample/runMain termflow.run.TermFlowMain"

# Echo app
sbt "termflowSample/runMain termflow.apps.echo.EchoApp"

# Counters
sbt "termflowSample/runMain termflow.apps.counter.SyncCounter"
sbt "termflowSample/runMain termflow.apps.counter.FutureCounter"

# Clock
sbt "termflowSample/runMain termflow.apps.clock.DigitalClock"
```

## Architecture

TermFlow is a functional TUI framework using an Elm-like architecture:

### Core Loop (TuiRuntime)
- `TuiRuntime.run()` takes a `TuiApp` and drives the main loop
- Uses a `CmdBus` (blocking queue) to coordinate messages between the app and async subscriptions
- Runs in an alternate terminal buffer with ANSI rendering

### TuiApp Trait
Apps implement `TuiApp[Model, Msg]` with four methods:
- `init(ctx)` → initial `Tui[Model, Msg]` (model + command)
- `update(model, msg, ctx)` → state transition returning new `Tui[Model, Msg]`
- `view(model)` → `RootNode` (virtual DOM)
- `toMsg(input)` → parse prompt input into a `Msg`

### Commands (Cmd)
- `NoCmd` - no action
- `Exit` - terminate the app
- `GCmd[Msg]` - dispatch a message
- `FCmd[A, Msg]` - async Future with result mapper
- `TermFlowErrorCmd` - surface errors to renderer

### Subscriptions (Sub)
Event streams that push `Cmd`s to the bus:
- `Sub.Every` - timer-based polling
- `Sub.InputKey` - keyboard input (JLine-based)
- `Sub.TerminalResize` - dimension change detection

### Virtual DOM (vdom.scala)
- `RootNode` - top-level container with optional `InputNode`
- `TextNode` - styled text at coordinates
- `BoxNode` - container with border support
- `InputNode` - prompt with cursor position
- `Style` - colors, bold, underline, border

## Module Structure

- `modules/termflow` - the library (published)
- `modules/termflow-sample` - demo apps (not published)

## Scala Versions

This branch (`main`) is Scala 3 only.
The `legacy-213-track` branch is the Scala 2.13 maintenance line.
Applicable fixes and critical updates are ported from `main` to `legacy-213-track`.
