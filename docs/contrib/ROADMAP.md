# TermFlow Roadmap

> Status: 2026-04-29 · Current release: **0.2.0** · Working towards **1.0**.
>
> Stages 1–3 are complete. Stage 4 (1.0 stabilisation) is in progress.
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
  parsing, SIGWINCH-driven resize.
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
- **~20 sample apps** — counter, async counter, clock, dashboard,
  echo, hello, forms, wizard, dialog, file-dialog, themes, unicode,
  stress, sine, hub, input, tabs, task, catalog, widgets, showcase.
- **Docs site** — live at `https://llm4s.github.io/termflow` with
  Introduction, four tutorials, seven layer guides, eight cookbook
  recipes, and aggregated Scaladoc.

No outstanding architectural debts for 1.0. Remaining work is in §3.

---

## 3. Stage 4 — Stabilisation (target: 1.0.0)

The lock-in stage. Goal: produce an API stable enough to keep
unchanged for years.

### 3.1 MiMa for binary compatibility

Add `sbt-mima-plugin`. Configure each of `termflow-terminal`,
`termflow-screen`, `termflow-app`, `termflow-widgets`, and
`termflow-testkit` to check against the previous release. Fail CI on
incompatible changes; require explicit `mimaBinaryIssueFilters` for
accepted breaks.

The decisions made via filters drive content for the migration guide
(§3.5).

### 3.2 Sample app catalogue gaps

Target: 25 apps; close to 20 today. Specific gaps remaining:

- **`tree/`** — file-tree explorer, natural fit on top of
  `fileDialog` and the `Tree` widget.
- **`editor/`** — minimal multi-file text editor, exercises
  `MultiLineInput` + `SplitPane` + `MenuBar` together.
- **`chat/` expansion** — extend the existing `chat` sample to use
  streaming + scrollback (see the
  [streaming-output cookbook recipe](../cookbook/streaming-output.md)).

### 3.3 Killer demo

A working `llm4s` chat client in <200 lines of TermFlow, used as the
README headline screenshot. Demonstrates streaming, dialogs, theming,
mouse, async tool calls — every Stage 1–3 capability in one app.

### 3.4 GridLayout + BorderLayout

Listed as Stage 1 work but never shipped — only `Row` / `Column` /
`Fill` / `Zone` exist today. Add:

- **`Layout.Grid(rows, cols, gap, children)`** — fixed-grid layout
  with span support. Lanterna's `GridLayout` is the template.
- **`Layout.Border(top, left, center, right, bottom)`** — five-zone
  border layout. Useful for "header / sidebar / main / footer"
  apps without manually computing heights.

Both should fit cleanly behind `Layout.resolveTo` (the size-aware
resolution) so existing `resolve` callers don't need changes.

### 3.5 Migration guide

Currently a placeholder. Populate as MiMa filters accumulate:

- One section per accepted incompatible change between 0.2.0 and 1.0.
- Before / after code recipes for each.
- A rationale paragraph for any non-obvious change.

If we don't break anything between now and 1.0, the page becomes a
one-liner ("no migration needed; 1.0 is binary-compatible with
0.2.0"). That's a fine outcome.

### 3.6 Definition of done for 1.0

- ☐ MiMa passes; published artefacts are stable.
- ☑ Docs site live at `https://llm4s.github.io/termflow`.
- ☑ Tutorial ladder complete (Hello World, Counter, Async, Forms).
- ☐ Sample app count ≥ 20 (need ~5 more after §3.2).
- ☐ README headline screenshot is the chat client.
- ☐ Migration guide populated (or "no migration needed" confirmed).

---

## 4. Stage 5 — Alternative backends (post-1.0, speculative)

Each backend is an opt-in module. None are on the 1.0 critical path.

### 4.1 Telnet backend

`termflow-backend-telnet`. Bind a port, accept connections, each
connection becomes a `TerminalBackend`. Telnet option negotiation
(NAWS for size, ECHO suppression, binary). Small library; medium
effort.

**Use case.** Self-hosted admin consoles, MUDs, BBS-style apps, oncall
debug shells. **Not encrypted** — operators wrap it in stunnel /
WireGuard / SSH-jumphost for production.

### 4.2 Web backend (xterm.js over WebSocket)

`termflow-backend-web`. Serve `xterm.js` plus a WebSocket; the WS
frames become the terminal stream. Run a TUI in a browser tab.

**Effort.** Large but cleanly bounded. Probably the most
"wow factor" of the bunch — a Scala TUI in the browser is unique.

### 4.3 Explicitly *not* planned (third-party PRs welcome)

- **Swing / AWT emulator backend** — desktop window with a TUI
  emulator inside. Skipped because §4.2 covers most of the same use
  cases at similar effort and reaches more users.
- **SSH backend** — wrap Apache MINA SSHD or sshj. Skipped because
  key management, auth, and connection state are a perpetually-supported
  surface area we don't want to own. Compose §4.1 Telnet behind an
  external SSH jump host instead.

The `TerminalBackend` trait stays public so external contributors
can ship either as a separate artefact.

---

## 5. Open questions

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

## 6. Lanterna comparison reference

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

## 7. Recent decisions (rolling, last ~3 months)

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
  (now §4.3); Telnet (§4.1) and Web (§4.2) remain.
