# termflow

[![Maven Central](https://img.shields.io/maven-central/v/org.llm4s/termflow_3.svg?label=Maven%20Central)](https://central.sonatype.com/artifact/org.llm4s/termflow_3)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://mit-license.org/)

📖 **[User guide & tutorials → llm4s.github.io/termflow](https://llm4s.github.io/termflow)**

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

- `modules/termflow`: the library (`org.llm4s:termflow_3`)
- `modules/termflow-testkit`: deterministic test harness — `TuiTestDriver`,
  golden-snapshot support, `TestRuntimeCtx` (`org.llm4s:termflow-testkit_3`,
  depend on as `% Test`)
- `modules/termflow-sample`: demo apps (not published)

## Quick Start

Add to your build (Scala 3):

```scala
libraryDependencies += "org.llm4s" %% "termflow" % "0.2.0"
```

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
- CI-equivalent local check: `sbt ciCheck`
- Scalafix rewrite: `sbt scalafixAll`
- Tests: `sbt test`
- Library coverage report: `sbt coverageLib`
- Pre-PR gate (format, scalafix, tests, coverage, sample smoke): `sbt prePR`
- Publish locally (for integration testing): `sbt publishLocal`

## Scala 3 Conventions

- Prefer `enum` for closed ADTs.
- Prefer `given` / `using` over implicit parameters and values.
- Prefer `extension` methods over implicit classes.
- Avoid implicit conversions; return explicit `Tui` values (for example, `model.tui`).
- Keep migration changes behavior-preserving unless a PR states otherwise.

## Async Work

`termflow` stays effect-system-agnostic. Async commands use `scala.concurrent.Future`
combined with the framework's `Result[A] = Either[TermFlowError, A]`, exposed as a
type alias that mirrors the [llm4s](https://github.com/llm4s/llm4s) core 1:1 so values
flow between the two libraries without an adapter:

```scala
type AsyncResult[+A] = Future[Result[A]]
```

Lift one onto the command bus with `Cmd.asyncResult`:

```scala
import termflow.tui.*
import termflow.tui.TuiPrelude.*

enum Msg:
  case Loaded(value: User)
  case Failed(err: TermFlowError)

def fetch(id: UserId): AsyncResult[User] = ...

Cmd.asyncResult(
  task      = fetch(id),
  onSuccess = Msg.Loaded.apply,
  onError   = Msg.Failed.apply,
  onEnqueue = Some(Msg.LoadingFlash)   // optional
)
```

`Future` failures (network drops, JVM exceptions) surface through the runtime's
standard `Cmd.TermFlowErrorCmd` path automatically; only domain errors need an
explicit `onError`. `AsyncResult` ships a small companion with `success`,
`failure`, `fromResult`, and `fromFuture` for lifting at the boundary.

We do **not** ship cats-effect or ZIO adapter modules — apps that use `IO` /
`ZIO` can bridge to a `Future` at the `Cmd` boundary in a couple of characters
of glue.

## Versioning

Versioning is fully driven by git tags via `sbt-dynver`; nothing is hand-edited
in `build.sbt` or a `version.sbt` file.

- A clean checkout of a `vX.Y.Z` tag → version `X.Y.Z` (release).
- Any commit past the latest tag → `X.Y.Z+<N>-<sha>-SNAPSHOT` (snapshot).
- No tags reachable → `0.0.0-UNKNOWN` (CI fallback only).

We follow [early SemVer](https://www.scala-lang.org/blog/2021/02/16/preventing-version-conflicts-with-versionscheme.html)
(`versionScheme := "early-semver"`): in `0.y.z`, a minor bump (`0.1.x → 0.2.0`)
may include breaking changes; patch bumps stay binary-compatible.

### Cutting a release

Releases are published to Maven Central by the `Release` GitHub workflow,
which fires on any pushed tag matching `v[0-9]*`:

```bash
git tag v0.2.1
git push origin v0.2.1
```

The workflow runs `sbt ci-release`, which:

1. Re-runs CI checks.
2. Imports the GPG key from `PGP_SECRET` and signs all artifacts.
3. Stages to the Sonatype Central Portal using `SONATYPE_USERNAME` / `SONATYPE_PASSWORD`
   (these are the **portal user token** values, not your Sonatype account login).
4. Releases the staged bundle automatically — no manual “close & release” step.

Artifacts land at `https://repo1.maven.org/maven2/org/llm4s/termflow_3/`
within a few minutes of the workflow finishing.

### Snapshots

Untagged commits on `main` are not auto-published. If you need a snapshot to
test downstream, either tag it (`v0.2.1-RC1`) or run `sbt publishLocal` and
depend on the locally-installed coordinate.

> Note: the legacy `search.maven.org` Solr index is not updated for projects
> publishing through the new Sonatype Central Portal. Use
> [central.sonatype.com](https://central.sonatype.com/artifact/org.llm4s/termflow_3)
> or the raw repo URL above to verify a release.
