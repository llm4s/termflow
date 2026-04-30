# TermFlow Roadmap

> Status: 2026-04-30 · Current release: **0.2.0** · Working towards **1.0**.
>
> Stages 1–3 are complete. Stage 4 (1.0 stabilisation) is in progress —
> §3.2 (sample apps) and §3.4 (Grid + Border layouts) have landed; the
> killer demo, the migration guide, and the pre-1.0 release-hardening
> checklist (§4) remain. MiMa (§3.1) was deferred to the post-1.0 cycle
> (baseline = 1.0.0).
> This document is forward-looking: it describes the work *left*, not the
> history of the work *done*.

---

## 1. Vision

> **Pure-FP TUIs for Scala. Deterministic, golden-tested, type-safe — with
> mouse, themes, and modal dialogs when you need them.**

TermFlow's defensible niche is **the Elm Architecture done well in
Scala**: immutable model, pure `update`, declarative `view`, async via
`Cmd`/`Sub`, plus a snapshot test harness no other Scala TUI library
offers.

### Non-goals

- **A Lanterna port.** No mutable widgets, no shared GUI thread, no
  listener callbacks as the primary event mechanism.
- **A general-purpose toolkit.** TermFlow targets interactive CLIs,
  REPLs, dashboards, installers, chat clients, and admin tools — not
  full-screen text editors or game engines.
- **Curses parity.** Just enough primitives to render and diff
  correctly.

---

## 2. Current state (0.2.0)

What ships today:

- **Five published modules** — `termflow-terminal` / `-screen` /
  `-app` / `-widgets` plus the umbrella `termflow` artefact, and
  `termflow-testkit` for tests. Maven Central as `org.llm4s::*`.
- **Elm-style runtime** — `TuiApp[Model, Msg]`, `Cmd`, `Sub`,
  `RuntimeCtx`, 60-fps frame-diffed ANSI renderer with capability
  downgrade.
- **Capability detection** — true-colour / 256-colour / 16-colour /
  8-colour / mono, bracketed paste, mouse (SGR-1006), extended modifier
  parsing, SIGWINCH-driven resize, terminal-attention notifications
  (iTerm2 OSC 9 / 1337, kitty OSC 99, VTE OSC 777, BEL fallback).
- **20+ widgets** — `TextField`, `MultiLineInput`, `Button`,
  `CheckBox`, `RadioGroup`, `Select`, `Autocomplete`, `ListView`,
  `Table`, `Tree`, `Tabs`, `SplitPane` (drag-resize), `Separator`,
  `ScrollBar`, `ProgressBar`, `Spinner`, `StatusBar`, `LogView`,
  `MenuBar`, `Form`. Plus `Layout` DSL (`Row` / `Column` / `Fill` /
  `Zone`), `HitTest[Id]`, `Theme` + `BorderChars`.
- **Seven dialog helpers** — `message`, `confirm`, `textInput`,
  `listSelect`, `waiting`, `fileDialog`, `directoryDialog`,
  `actionList`.
- **Grapheme-aware text editing** — UAX #29 cluster boundaries via
  `BreakIterator`, wide-cell math via `WCWidth`.
- **Testkit** — `TuiTestDriver`, `KeySim`, `MouseSim`,
  `GoldenSupport`. Published as `termflow-testkit`.
- **~22 sample apps** — counter, async counter, clock, dashboard,
  echo, hello, forms, wizard, dialog, file-dialog, themes, unicode,
  stress, sine, hub, input, tabs, task, catalog, widgets, showcase,
  tree, editor, chat.
- **Docs site** — live at `https://llm4s.github.io/termflow` with
  Introduction, four tutorials, seven layer guides, nine cookbook
  recipes, and aggregated Scaladoc.

No outstanding architectural debts for 1.0. Remaining work is in §3
and the pre-1.0 release-hardening checklist in §4.

---

## 3. Stage 4 — Stabilisation (target: 1.0.0)

The lock-in stage. Goal: produce an API stable enough to keep
unchanged for years.

### 3.1 MiMa for binary compatibility — deferred to post-1.0

Originally planned as a 1.0 gate, but moved out of the critical path
(decision: 2026-04-30). The simpler workflow is to ship 1.0 first, then
wire `sbt-mima-plugin` against `1.0.0` as the baseline so every
subsequent release (`1.0.x`, `1.1.0`, …) is checked.

Why the change:

- 0.2.0 is already API-stable, so the 0.2.0 → 1.0 filter list would
  mostly be noise.
- Avoids the bookkeeping cost of maintaining
  `mimaBinaryIssueFilters` during the 0.2.x → 1.0 window.
- Removes the chicken-and-egg between §3.1 and §3.5 — see §3.5 for
  the simplified migration-guide stance.

Plan once 1.0 ships:

1. Add `sbt-mima-plugin` to `project/plugins.sbt`.
2. Set `mimaPreviousArtifacts := Set("org.llm4s" %% name % "1.0.0")`
   on each of `termflow-terminal`, `termflow-screen`, `termflow-app`,
   `termflow-widgets`, `termflow`, and `termflow-testkit`.
3. Append `mimaReportBinaryIssues` to the `ciCheck` alias.

This becomes part of the 1.0.1 / 1.1.0 release prep, not 1.0.

### 3.2 Sample app catalogue gaps — ☑ landed

All three planned samples shipped (#188 / #189 / #190): `tree/`
(file-tree explorer over the `Tree` widget), `editor/` (multi-buffer
text editor exercising `MultiLineInput` + `SplitPane` + `MenuBar`),
and `chat/` (streaming chat with scrollback per the
[streaming-output cookbook recipe](../cookbook/streaming-output.md)).
Catalogue now stands at 22 apps.

### 3.3 Killer demo

A working `llm4s` chat client in <200 lines of TermFlow, used as the
README headline screenshot. Demonstrates streaming, dialogs, theming,
mouse, async tool calls — every Stage 1–3 capability in one app.

### 3.4 GridLayout + BorderLayout — ☑ landed

Shipped in #187. `Layout.Grid(columns, rowGap, colGap, cells)` with
`GridCell` row/column spans, and `Layout.Border(top, left, center,
right, bottom)` with five-zone resolution. Both flow through
`Layout.resolveTo` so existing `resolve` callers are unchanged.
`LayoutGridSpec` and `LayoutBorderSpec` cover sizing, gaps, spans,
and zone omission.

### 3.5 Migration guide

With MiMa deferred (§3.1), there is no automated source of breakage
data for 0.2.0 → 1.0. The guide page stays useful as a hand-written
record of any deliberate API changes during the run-up to 1.0.

For 1.0:

- Walk the public API by hand, note any intentional breaks, and write
  before/after recipes for each.
- If nothing broke, collapse the page to "no migration needed; 1.0 is
  source-compatible with 0.2.0."
- Once §3.1 is wired post-1.0, future entries are driven by MiMa
  filters as originally intended.

### 3.6 Definition of done for 1.0

- ☑ Docs site live at `https://llm4s.github.io/termflow`.
- ☑ Tutorial ladder complete (Hello World, Counter, Async, Forms).
- ☑ Sample app count ≥ 20 (22 today, including `tree`, `editor`, `chat`).
- ☑ `Layout.Grid` + `Layout.Border` shipped (§3.4).
- ☐ README headline screenshot is the chat client (§3.3).
- ☐ Migration guide populated (or "no migration needed" confirmed) (§3.5).
- ☐ Pre-1.0 release-hardening checklist complete (§4).

MiMa (§3.1) was originally on this list; it is now scheduled for the
1.0.1 / 1.1.0 cycle with `1.0.0` as the baseline.

---

## 4. Stage 5 — Pre-1.0 release requirements

Final hardening before tagging `v1.0.0`. These are not new feature
tracks; they are release-quality gates for the current library surface.

### 4.1 User-visible error path

`Cmd.TermFlowErrorCmd` must be visible in the default runtime/renderer
path. Validation failures, rejected prompt input, and async failures
should render a deterministic transient error view (or documented
equivalent) instead of disappearing.

Acceptance:

- Default `TuiRuntime.run(..., SimpleANSIRenderer())` surfaces
  `TermFlowError` to the user.
- Testkit can assert the same error path without a real terminal.
- Docs describing validation/error behaviour match the implementation.

### 4.2 Version and release-doc sweep

Before the final release branch/tag, every copy-pasteable coordinate
and release statement should reflect the intended 1.0 release story.

Acceptance:

- README, install guide, API reference, migration notes, and roadmap
  agree on the current released baseline and the next target.
- Release instructions describe the exact tag/workflow path for
  `v1.0.0`.
- Any stale `0.2.0` / `0.3.0` examples are intentional and explained.

### 4.3 Public API and docs example audit

Do a final pass over the public-facing APIs and every tutorial/guide
snippet. The goal is not to freeze internals forever; it is to remove
surprise from the surface users will copy into their apps.

Acceptance:

- Public names, signatures, and examples line up (`Cmd.asyncResult`,
  widget constructors, `Sub.TerminalResize`, layout helpers, etc.).
- Any deliberately sharp/advanced APIs are marked as SPI or documented
  with constraints.
- Migration notes list every known user-visible API adjustment, or
  explicitly say no migration is required.

### 4.4 Layout ergonomics audit

Review the final layout API for common misuse before 1.0 locks it in.
In particular, make the distinction between eager `Layout.resolve` /
`toRootNode` and budget-aware `RootNode(layout = Some(...))` obvious
enough that users do not accidentally disable `Fill`/resize behaviour.

Acceptance:

- Either `Layout.toRootNode` preserves budget-aware layout semantics,
  or docs/examples make the eager-vs-deferred distinction explicit.
- At least one tutorial or cookbook recipe demonstrates the preferred
  full-screen/resizable layout pattern.
- Golden or unit tests cover the intended public pattern.

### 4.5 Rolling console / agent UI recipe

TermFlow should make the Claude Code / Cursor-style transcript pattern
obvious: execution history scrolls upward, new output auto-tails while
the user is at the bottom, and the prompt remains fixed at the bottom
of the viewport.

Acceptance:

- Add a cookbook recipe for a rolling console / agent UI built from
  `widgets.LogView`, `Prompt`, and a bottom-row `InputNode`.
- Document the supported model clearly: TermFlow owns an in-app
  scrollback viewport in the alternate buffer; native terminal
  scrollback is not the default runtime behaviour.
- The recipe covers auto-tail, pausing auto-tail when the user scrolls
  up, resuming with `End`, and bounding retained history.
- Link the recipe from the install/intro path or widgets guide so LLM
  and command-runner app authors can find it quickly.

### 4.6 Mouse-wheel scrolling for LogView-style views

Mouse wheel input is already decoded as `InputKey.Mouse(MouseEvent.Scroll(...))`.
The rolling-console path should demonstrate and, where useful, smooth
over the app wiring needed to turn that into `scrollOffset` changes.

Acceptance:

- `chatDemo` handles mouse-wheel up/down over the transcript pane.
- Add a small helper or documented pattern for mapping
  `MouseEvent.Scroll` to `LogView` scroll deltas while ignoring scrolls
  outside the target viewport.
- Add deterministic test coverage using `MouseSim.scrollUp` /
  `MouseSim.scrollDown`.
- The docs mention keyboard equivalents for environments where mouse
  reporting is unavailable.

### 4.7 Test coverage review and quick wins

Because TermFlow is mostly deterministic UI logic, coverage should be
higher than a typical terminal integration project. Run `sbt --batch
coverageLib`, inspect the lowest-covered files/branches, and take the
low-risk wins before 1.0.

Current local coverage snapshot (2026-04-30):

- `termflow-terminal`: 66.04% statements / 63.32% branches.
- `termflow-screen`: 71.81% statements / 65.22% branches.
- `termflow-app`: 80.29% statements / 68.61% branches.
- `termflow-widgets`: 92.63% statements / 85.00% branches.

Acceptance:

- Add focused tests for pure/render/update logic with obvious gaps.
- Prefer deterministic tests over brittle real-terminal integration.
- Record any accepted low-coverage areas with rationale (JLine/raw TTY
  integration, shutdown-hook paths, genuinely platform-specific code).
- Keep `coverageLib` green and ensure the combined trend moves up, with
  particular attention to `termflow-terminal` and `termflow-screen`.

### 4.8 Zero-warning build and Scaladoc polish

The 1.0 branch should build cleanly enough that new warnings stand out.

Acceptance:

- `sbt --batch ciCheck` emits no Scala compiler warnings.
- `sbt --batch unidoc` completes without unresolved-link warnings where
  a simple Scaladoc link fix is available.
- mdBook/linkcheck remain green.

---

## 5. Stage 6 — Alternative backends and renderers (post-1.0, speculative)

Each backend or renderer is opt-in. None are on the 1.0 critical path.

### 5.1 Telnet backend

`termflow-backend-telnet`. Bind a port, accept connections, each
connection becomes a `TerminalBackend`. Telnet option negotiation
(NAWS for size, ECHO suppression, binary). Small library; medium
effort.

**Use case.** Self-hosted admin consoles, MUDs, BBS-style apps, oncall
debug shells. **Not encrypted** — operators wrap it in stunnel /
WireGuard / SSH-jumphost for production.

### 5.2 Web backend (xterm.js over WebSocket)

`termflow-backend-web`. Serve `xterm.js` plus a WebSocket; the WS
frames become the terminal stream. Run a TUI in a browser tab.

**Effort.** Large but cleanly bounded. Probably the most
"wow factor" of the bunch — a Scala TUI in the browser is unique.

### 5.3 Rolling console renderer

A constrained normal-buffer renderer/runtime mode for agent and command
runner apps that want native terminal scrollback: output appends to the
terminal's real history while a live prompt/status area remains pinned
near the bottom.

This should not try to support arbitrary full-screen TermFlow VDOM. The
current default runtime enters the alternate buffer and the default
renderer diffs fixed frames by absolute coordinates; native scrollback
needs a different contract built around append-only transcript events,
prompt repainting, cursor save/restore, and possibly terminal scroll
regions.

**Use case.** Claude Code / Cursor-style agents, build runners, REPLs,
and long-running command logs where users expect their terminal
emulator's own scrollbar, copy/search behaviour, and shell history
context to keep working.

**Shape.** Likely a dedicated `RollingConsoleApp` or renderer API, not
a flag on `SimpleANSIRenderer`. It should support bounded app-side
history for replay/testing, normal-buffer append, fixed bottom prompt,
keyboard input, resize handling, and a graceful fallback when terminals
handle scroll regions poorly.

**Effort.** Medium-large and compatibility-sensitive. Worth prototyping
after 1.0, but too risky to make part of the 1.0 contract.

### 5.4 Explicitly *not* planned (third-party PRs welcome)

- **Swing / AWT emulator backend** — desktop window with a TUI
  emulator inside. Skipped because §5.2 covers most of the same use
  cases at similar effort and reaches more users.
- **SSH backend** — wrap Apache MINA SSHD or sshj. Skipped because
  key management, auth, and connection state are a perpetually-supported
  surface area we don't want to own. Compose §5.1 Telnet behind an
  external SSH jump host instead.

The `TerminalBackend` trait stays public so external contributors
can ship either as a separate artefact.

---

## 6. Open questions

1. **Native image.** `sbt-native-image` build for
   `graalvm-native-image`? The shutdown hook + JLine reflection make
   this non-trivial. Investigate post-1.0.
2. **Cross-publish for Scala 2.13.** Already on `legacy-213-track`.
   Keep parity through 1.0, then re-evaluate based on adoption.
3. **Resizing semantics.** When the terminal shrinks, do we clip or
   rewrap? Currently clip. The Layout pass makes rewrap cheap if we
   want to switch.
4. **i18n.** Right-to-left text? Bidi? Probably out of scope for
   1.0; document as a known limitation.
5. **Accessibility.** Screen readers don't really hit a TUI — they
   read the terminal directly. Worth an "Accessibility notes" docs
   section before 1.0.
6. **Windows native.** Currently relies on JLine for cmd.exe /
   Windows Terminal. May need a JNA-backed
   `WindowsConsoleBackend` if JLine's behaviour proves insufficient.

---

## 7. Lanterna comparison reference

The original 0.1.x roadmap was structured around a comparison with
[Lanterna](https://github.com/mabe02/lanterna). That comparison drove
Stages 1–3, and almost every Lanterna component now has a TermFlow
equivalent (`TextField` ≈ `TextBox`, `ListView` ≈ `ActionListBox`,
`SplitPane` ≈ `SplitPanel`, etc.). Two Lanterna things we deliberately
*don't* match:

- **The shared GUI thread / listener-callback event model.** Replaced
  by the Elm-style `update` loop. Not coming back.
- **Mutable widgets.** Replaced by stateless renderers + state on the
  app's model. Not coming back.

Two TermFlow-only wins worth preserving through 1.0:

- **Golden-snapshot testing** (`TuiTestDriver` + `GoldenSupport`).
- **`HitTest[Id]` cache** built from the layout pass — Lanterna has
  no equivalent.

---

## 8. Recent decisions (rolling, last ~3 months)

- *2026-04-30* — Terminal-attention notifications shipped:
  `Cmd.RequestAttention` and `Cmd.Notify(title, body)` with detection for
  iTerm2 (OSC 9 / 1337), kitty (OSC 99), VTE (OSC 777), and a BEL
  fallback. Override via `TERMFLOW_NOTIFICATIONS=off|bell|auto`. Wired
  through the showcase Help tab and a new `notifications` cookbook recipe.
- *2026-04-30* — Added rolling console renderer (§5.3) as a post-1.0
  idea: native terminal scrollback for agent / command-runner UIs via a
  constrained normal-buffer runtime, not the default full-screen renderer.
- *2026-04-30* — Added Stage 5 (§4) as the pre-1.0 release-hardening
  checklist: user-visible errors, release-doc accuracy, API/docs audit,
  layout ergonomics, rolling-console UX, mouse-wheel scrollback, coverage
  quick wins, and zero-warning builds.
- *2026-04-30* — MiMa (§3.1) deferred to post-1.0; baseline becomes
  `1.0.0`. Reason: 0.2.0 is already API-stable, so the 0.2.0 → 1.0
  filter list would be mostly noise, and decoupling §3.1 from §3.5
  removes a chicken-and-egg dependency.
- *2026-04-30* — Stage 4 §3.2 closed: `tree/` (#188), `editor/` (#189),
  and streaming `chat/` (#190) sample apps landed. Catalogue at 22.
- *2026-04-30* — Stage 4 §3.4 closed: `Layout.Grid` + `Layout.Border`
  shipped in #187 with `LayoutGridSpec` / `LayoutBorderSpec` coverage.
- *2026-04-29* — Docs site launched (Stage 4 §3 complete except for
  migration guide). mdBook + sbt-unidoc on GitHub Pages.
- *2026-04-28* — Stage 3 final components landed: `actionList`
  dialog, `ScrollBar`, `Separator`, `SplitPane` drag-resize,
  hit-test cache (`HitTest[Id]` + `Layout.Zone` +
  `resolveTracked`), grapheme-aware navigation, `MultiLineInput`.
- *2026-04-28* — `Cmd.FCmd` decision: stay on `scala.concurrent.Future`
  + `Result[A]`; no `cats-effect` / `zio` modules planned. Apps
  bridge to `Future` at the `Cmd` boundary.
- *2026-04-27* — Stages 1 and 2 closed. Module split shipped early
  as Stage 4 prep so MiMa filters can be wired per-module on day one.
- *2026-04-26* — Decision to deprioritise Swing/AWT and SSH backends
  (now §5.4); Telnet (§5.1) and Web (§5.2) remain.
