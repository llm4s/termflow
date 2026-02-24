# termflow

`termflow` is a small, functional terminal UI (TUI) framework for Scala.

It’s designed for building interactive CLIs with a simple architecture:

- a pure-ish `update` function (state transitions)
- a `view` function (render a small virtual DOM)
- `Cmd` for async work and `Sub` for event streams (keys, timers, etc.)

The project started as the TUI layer for LLM4s sample applications, but it is
usable on its own.

## What You Can Build

- prompt-driven apps (REPL-style, command palettes)
- streaming output (e.g., LLM token streaming)
- progress spinners and long-running tasks
- simple dashboards (lists, panes, status bars)

## Notes On Rendering

Rendering is intentionally simple today. Virtual-DOM diffing and throttling to
minimize flicker are work-in-progress topics we plan to iterate on (especially
for high-frequency updates like spinners and streaming text).

## Scala Versions

This branch (`main`) is the Scala 3 line.
The `legacy-213-track` branch is the Scala 2.13 maintenance line.
We intend to regularly port applicable fixes and critical updates from `main` to `legacy-213-track`.

## Modules

- `modules/termflow`: the library
- `modules/termflow-sample`: demo apps (not published)

## Quick Start

Run a sample app:

- `sbt "termflowSample/runMain termflow.run.TermFlowMain"`

## Sample Apps

The `termflow-sample` module contains a few small demo apps you can run with `runMain`.

- Echo: `sbt "termflowSample/runMain termflow.apps.echo.EchoApp"`
- Counter (sync): `sbt "termflowSample/runMain termflow.apps.counter.SyncCounter"`
- Counter (async + spinner): `sbt "termflowSample/runMain termflow.apps.counter.FutureCounter"`
- Clock: `sbt "termflowSample/runMain termflow.apps.clock.DigitalClock"`

Note: there are also small “inspector” utilities under `termflow.run.jline.*` to
debug key sequences and line editing behaviour.

## Build

- Compile: `sbt compile`
- Format: `sbt scalafmtAll`
- CI-equivalent local check: `sbt --batch ciCheck`
- Scalafix rewrite: `sbt scalafixAll`
- Tests: `sbt test`
- Library coverage report: `sbt --batch coverageLib`
- Publish locally (for integration testing): `sbt publishLocal`

## Scala 3 Conventions

- Prefer `enum` for closed ADTs.
- Prefer `given` / `using` over implicit parameters and values.
- Prefer `extension` methods over implicit classes.
- Avoid implicit conversions; return explicit `Tui` values (for example, `model.tui`).
- Keep migration changes behavior-preserving unless a PR states otherwise.

## Development Guidelines

For canonical style, PR scoping, and compiler option rationale, see:

- `docs/development-guidelines.md`

## Versioning

This repo uses `sbt-dynver` for versioning:

- tags like `v0.1.0` produce release versions (`0.1.0`)
- untagged commits use snapshot versions derived from git metadata
