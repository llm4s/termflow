# Render Pipeline

TermFlow now uses a coalesced runtime scheduler with framebuffer diff rendering to reduce flicker under high-frequency updates.

## Pipeline

```text
state events
  -> mark frame dirty
  -> coalesce queued commands
  -> build latest view (once per commit)
  -> materialize next frame buffer
  -> diff(previous, current)
  -> emit minimal ANSI patch
  -> restore logical input cursor
```

## Key Invariants

1. At most one render commit is active at a time.
2. Rendering is latest-state wins; intermediate states can be dropped during bursts.
3. Diff output includes cleanup for shrink cases (line tail and removed rows).
4. Cursor visibility is runtime-owned (startup/shutdown/interrupt), not frame-owned.
5. Each frame writes one buffered ANSI payload from renderer perspective.

## Why Flicker Dropped

- Most frames update a small subset of cells instead of repainting everything.
- Command bursts are coalesced before render, avoiding redundant intermediate paints.
- Cursor movement is deterministic and restored after body updates.

## Regression Focus Areas

- Prompt/input updates must not leave stale tail characters.
- Shrinking content must clear removed rows/columns.
- Borders at terminal edges must remain stable across rapid updates.
- Ctrl-C / unexpected exits must always restore cursor visibility.

## Snapshot Testing

The `termflow.testkit` package (published on the `test->test` classpath) drives a `TuiApp` synchronously — bypassing `TuiRuntime` — and lets tests compare the resulting `RenderFrame` against on-disk golden files. This is the primary regression safety net for the items above.

### Writing a snapshot test

```scala
import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.{GoldenSupport, TuiTestDriver}

class MyAppSnapshotSpec extends AnyFunSuite with GoldenSupport:
  test("initial frame"):
    val d = TuiTestDriver(MyApp.App, width = 40, height = 10)
    d.init()
    d.send(MyApp.Msg.DoSomething)
    assertGoldenFrame(d.frame, "after-do-something")
```

- `TuiTestDriver` exposes `model`, `frame`, `send(msg)`, `cmds`, `exited`, and `observedErrors`.
- Subscriptions (`Sub.Every`, `Sub.InputKey`, `Sub.TerminalResize`) are never started — tests stay deterministic. For prompt-driven apps, construct the wrapping `Msg` directly (e.g. `Msg.ConsoleInputKey(KeyDecoder.InputKey.CharKey('+'))`) instead of waiting on the input thread.
- `Cmd.FCmd` must wrap a pre-resolved `Future.successful(...)`; the driver will not block on unresolved futures.

### Golden file format

Goldens are stored under `src/test/resources/termflow/golden/<SuiteName>/<name>.golden` and use a pipe-wrapped, chars-only format:

```text
# width=40 height=13
# cursor=6,10
|┌──────────────────────────────────┐    |
|│Current count: 0                  │    |
...
```

The leading/trailing `|` markers make every row's width unambiguous and preserve trailing whitespace across editors and CI. Style/colour information is intentionally omitted — layout is what the render pipeline regresses on.

### Narrow assertions for prompt/cursor rows

Full-frame snapshots are brittle for tests that care about a single row (e.g. a prompt repaint regression). Use `assertGoldenString` with a one-row extract:

```scala
private def row(frame: RenderFrame, rowIndex: Int): String =
  val cells = frame.cells(rowIndex - 1)
  val sb    = new StringBuilder("|")
  (0 until frame.width).foreach(c => sb.append(cells(c).ch))
  sb.append("|").toString

assertGoldenString(row(d.frame, 7), "cursor-row-after-abc")
```

`InputLineReproSpec` uses this pattern to pin the exact cursor-math regressions behind #73 and #74.

### Updating goldens

Record or refresh a snapshot:

```bash
sbt -Dtermflow.update-goldens=true "termflowSample/testOnly *MyAppSnapshot*"
# or:
UPDATE_GOLDENS=1 sbt "termflowSample/testOnly *MyAppSnapshot*"
```

Always review the resulting `git diff` before committing. The `-Dtermflow.update-goldens=true` system property is forwarded into forked test JVMs by `build.sbt`.

### What snapshot tests catch

- Repaint regressions (stale cells left behind on shrink/clear)
- Cursor-position drift when the prompt viewport scrolls
- Layout drift when terminal dimensions change behaviour
- Border / text overflow interactions at box edges

They do not exercise timer-driven or random sources — those apps (`DigitalClock`, `SineWaveApp`, `FutureCounter`) are deliberately excluded from the golden suite.
