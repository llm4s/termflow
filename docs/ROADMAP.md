# TermFlow Roadmap

> Status: **draft proposal**, 2026-04-26. Maintained alongside the codebase;
> updated as decisions land. Items are not commitments — this is the plan we
> currently believe in, organised into release-shaped stages.

This document captures the medium-term direction for TermFlow, motivated by
a comparison with the Java [Lanterna](https://github.com/mabe02/lanterna)
library. It is intended to be detailed enough to act on without further
research: each stage lists the work, the rationale, the proposed shape of the
public API, and the open questions.

---

## 1. Vision

> **Pure-FP TUIs for Scala. Deterministic, golden-tested, type-safe — with
> mouse, themes, and modal dialogs when you need them.**

TermFlow's defensible niche is **the Elm Architecture done well in Scala**:
immutable model, pure `update`, declarative `view`, async via `Cmd`/`Sub`,
plus a snapshot test harness that no other Scala TUI library offers. Lanterna
is broader and battle-tested, but it is mutable and imperative end-to-end —
fundamentally a Java toolkit. We do not want to reimplement Lanterna. We want
to **borrow Lanterna's layering, breadth, and documentation discipline while
keeping the pure core**.

### Non-goals

- **A Lanterna port.** No mutable widgets, no shared GUI thread, no listener
  callbacks as the primary event mechanism.
- **A general "any-purpose" toolkit.** TermFlow targets interactive CLIs,
  REPLs, dashboards, installers, chat clients, and small admin tools — not
  full-screen text editors or game engines.
- **Curses parity.** We do not need every cursor primitive ncurses exposes.
  We need enough to render and diff correctly.

---

## 2. Current state (0.2.0)

### Strengths

- Clean Elm-style architecture (`TuiApp[Model, Msg]`, `Cmd`, `Sub`).
- Frame-diffed ANSI renderer with 60 FPS coalescing
  (`AnsiRenderer.scala`, `RENDER_PIPELINE.md`).
- Pluggable `TerminalBackend` trait (one production impl: JLine).
- Strong testing story: golden snapshot framework, deterministic
  `TuiTestDriver`.
- Reasonable widget set: `TextField`, `ListView`, `Table`, `Select`,
  `Button`, `ProgressBar`, `Spinner`, `StatusBar`.
- Devtools overlay (`Devtools.wrap`).
- Cross-published to Maven Central as `org.llm4s:termflow_3` /
  `_2.13`.

### Weaknesses (the "got it wrong" list)

These are the architectural debts to pay down before the API ossifies at 1.0.
See §4 for detailed discussion.

1. Absolute coordinates leak into `VNode` — `TextNode(x, y, …)`,
   `BoxNode(x, y, w, h, …)` (`vdom.scala:61-125`). The data model is a
   coordinate calculator masquerading as a layout system.
2. `Layout.scala` resolves to absolute coords at *build* time, not render
   time, so resize cannot reflow without rerunning `view`.
3. Widgets bake rendering. No theme/renderer separation.
4. No layer/overlay primitive — no answer for modal dialogs.
5. `TerminalBackend` is interface-only; no `VirtualTerminalBackend` exposed.
6. `Color` is 8-color only (no Indexed/RGB cases). Public API; breaking to
   change.
7. Resize is polled, not signalled.
8. Single module, single package — no narrative discoverability.
9. No tutorial ladder; design docs but no on-ramp.
10. No external docs site.

### Comparative reference

The Lanterna comparison feeding this roadmap is summarised inline below; the
full notes live at the bottom of the document (§9).

---

## 3. Stages overview

| Stage | Versions | Theme | Status |
|---|---|---|---|
| 1 | 0.3.x | **Foundational refactor**: layout, layers, theme split, capabilities | done (2026-04-27) |
| 2 | 0.4.x | **Capability expansion**: mouse, 256/truecolor, unicode width, paste | done (2026-04-27) |
| 3 | 0.5.x | **Breadth**: dialog helpers, more widgets, testkit module | in progress |
| 4 | 1.0.0 | **Stabilise**: module split, MiMa, docs site, three-layer narrative | proposed |
| 5 | post-1.0 | **Alternative backends**: Swing emulator, telnet/SSH | speculative |

The first three stages each break binary compat; that is the deliberate
purpose of staying at 0.x. Stage 4 is the lock-in point.

---

## 4. Stage 1 — Foundational refactor (target: 0.3.0) — **done 2026-04-27**

**Goal**: fix the architectural debts that would be expensive to repair after
1.0. Stage 1 is invasive but small in surface area; it is internal plumbing
plus a few public-API breaks.

**Outcome.** All sub-items landed across PRs #157–#164:

| Sub-item | Lands in | Status |
|---|---|---|
| §4.1 Relative-coordinate VDom | `Layout.Fill` + render-time resolution via `RootNode.layout` (#162) | done — `Layout` is the structural API; positioned `VNode` retained as escape hatch |
| §4.2 Layer / overlay system | `Overlay` primitive + `Dialogs.confirm/message` (#160) | done |
| §4.3 Theme + WidgetRenderer split | `Theme` + themable `BoxNode.chars` (#159, plus existing `Theme.scala`) | done — `Theme` ships with `BorderChars` slots; per-widget renderers come in Stage 3 with the dialog/widget expansion |
| §4.4 Color depth + capability detection | `Color.Indexed` / `Color.Rgb` + capability-driven downgrade (#157) | done |
| §4.5 SIGWINCH | `Sub.TerminalResize` switched to `backend.onResize` signal with polling fallback (#158) | done |
| §4.6 Module split | `termflow-testkit` promoted to a published artifact (#161) | partial — testkit is its own module; the finer-grained `termflow-terminal/screen/app/widgets` carve-out is deferred to Stage 4 (§7.1), where MiMa will be wired up at the same time |
| §4.7 Definition of done | Stage 1 showcase demo (`sbt showcase`, #163) exercises layout, overlays, theme, color depth, and resize end-to-end | done |

The §4.6 module split was scoped down deliberately: the user-facing pain of
"no separate testkit" was real (#147) and is now fixed; the terminal/screen/app
split is internal plumbing that is cheap to do later, and pairs naturally with
Stage 4 stabilisation when MiMa filters need to be defined per-module anyway.


### 4.1 Relative-coordinate VDom (P0, large)

**Problem.** Today `VNode` types carry absolute `(x, y)`. Composition is
fragile: a child cannot be repositioned without computing new coordinates
based on the parent. `Layout.Column`/`Row` (`Layout.scala`) is a pre-pass
that *bakes* coordinates into the tree, so the renderer receives a flat
positioned list rather than a structure it can reflow.

**Proposed shape.** `VNode` becomes structural; coordinates appear only as
the result of a layout pass that runs in the renderer.

```scala
sealed trait VNode

final case class Text(runs: Vector[StyledRun]) extends VNode
final case class Box(content: VNode, border: Option[Border], style: Style) extends VNode
final case class Linear(direction: Direction, gap: Int, children: Vector[VNode]) extends VNode
final case class Grid(rows: Vector[Vector[VNode]], gap: (Int, Int)) extends VNode
final case class Stack(layers: Vector[VNode]) extends VNode  // overlapping
final case class Sized(min: Size, max: Size, content: VNode) extends VNode
final case class Input(prompt: Text, cursor: Cursor, style: Style) extends VNode
final case class Empty(width: Int, height: Int) extends VNode
```

A layout pass walks this tree, allocates a rectangle to each node based on
its parent's contract, and emits a positioned cell stream into the renderer.
This is the same shape Lanterna's `LinearLayout`/`GridLayout`/`BorderLayout`
produce, but as data, not mutation.

**Migration path.** Keep the old positioned-node API behind
`termflow.tui.legacy.*` for one minor; deprecate; remove at 0.4.0.

**Risks.** Layout passes need to handle minimum sizes, fill behaviour, and
weighted distribution. We probably want to copy Lanterna's `LinearLayout`
weights idea (each child has a `Fixed`, `Fill`, or `Weight(n)` policy).

### 4.2 Layer / overlay system (P0, medium)

**Problem.** No way to render a modal dialog over the current screen. The
devtools overlay is bespoke (`Devtools.wrap`).

**Proposed shape.** A `Layer` is a `(TuiApp, InputCapture)` pair. The
runtime owns a stack; rendering composites bottom-up; input is offered
top-down until consumed.

```scala
enum InputCapture:
  case Capture           // consume all input, do not pass through
  case Modal             // capture but block underlying timers
  case Passthrough       // see input, never consume

final case class Layer[Model, Msg](
  app: TuiApp[Model, Msg],
  capture: InputCapture,
  position: LayerPosition  // Centered | TopRight | FullScreen | At(x,y)
)

trait RuntimeCtx[Msg]:
  def pushLayer[L_Model, L_Msg](layer: Layer[L_Model, L_Msg], onResult: L_Model => Msg): Unit
  def popLayer(): Unit
```

This is a pure-FP analogue of Lanterna's `MultiWindowTextGUI` with
`Window.Hint.MODAL`/`CENTERED`/`NO_DECORATIONS`. No mutable window state —
the layer's own model is the truth.

**Devtools migration.** `Devtools.wrap` becomes `Devtools.layer`, a stock
`Passthrough` layer. Removes ~200 lines of bespoke wrapping code.

**Open questions.**
- How does a child layer return a result to its parent? (Probably an
  `onResult` callback at push time, mapping the child's final model to a
  parent message.)
- Do timers in covered layers continue ticking? (Default: yes for
  `Capture`, no for `Modal`.)
- Animation when pushing/popping? (Probably out of scope; we're not a
  graphical toolkit.)

### 4.3 Theme + WidgetRenderer split (P1, medium)

**Problem.** Widgets bake their rendering. `Button.view` directly returns
styled nodes; the only way to restyle is to fork.

**Proposed shape.** Three concepts:

```scala
trait Theme:
  def colors: ColorPalette
  def chars: BorderChars
  def widget: WidgetThemes  // per-widget styling slots

trait WidgetRenderer[State]:
  def render(state: State, theme: Theme, focus: Boolean): VNode

object Button:
  def renderer: WidgetRenderer[Button.State] = …  // default
  def render(state: State)(using Theme, WidgetRenderer[Button.State]): VNode
```

Themes ship as values (no `.properties` loader at first; KISS). Default
themes: `Theme.minimal`, `Theme.rounded`, `Theme.contrast`. Users can
copy and tweak.

**API stability.** `using Theme` defaults to `Theme.minimal` so existing
call sites compile.

### 4.4 Color depth + capability detection (P0, small)

**Problem.** `Color` enum is 8-color only. `Style` has no path for indexed
or truecolor. Public API.

**Proposed shape.**

```scala
enum Color:
  case Default
  case Named(c: NamedColor)              // 8 ANSI
  case Indexed(n: Int)                   // 0..255
  case Rgb(r: Int, g: Int, b: Int)       // 24-bit

object Capabilities:
  enum ColorDepth:
    case Mono, Ansi8, Ansi16, Indexed256, Truecolor

  def detect(env: Map[String, String]): Capabilities
  // probes COLORTERM, TERM, NO_COLOR, etc.

trait TerminalBackend:
  def capabilities: Capabilities
  // …
```

The renderer downgrades unsupported colors gracefully (truecolor → nearest
indexed → nearest 8). NO_COLOR honoured per spec.

### 4.5 SIGWINCH (P1, small)

Replace `Sub.TerminalResize`'s polling with JLine's
`Terminal.handle(Signal.WINCH, …)`. Keep the `Sub` interface; only the
implementation changes. Falls back to polling on backends that lack signal
support.

### 4.6 Module split (P1, small)

Split the single `modules/termflow` artifact into:

- `termflow-terminal` — `TerminalBackend`, capabilities, key decoding.
- `termflow-screen` — `RenderFrame`, diff, ANSI emission.
- `termflow-app` — `TuiApp`, `Cmd`, `Sub`, `Layer`, runtime.
- `termflow-widgets` — `Button`, `TextField`, etc.
- `termflow-testkit` — `TuiTestDriver`, golden support, virtual backend.

Aggregator `termflow` re-exports for migration. Mirrors Lanterna's
`terminal/` ↔ `screen/` ↔ `gui2/` package layout, and lets users depend on
just `termflow-app` if they want to roll their own widgets.

### 4.7 Definition of done for Stage 1

- `VNode` carries no absolute coordinates in user-facing code.
- Layout pass runs at render time; resize reflows without rerunning `view`.
- Layer system in place; devtools migrated; sample dialog implemented.
- `Theme` is a parameter; ≥ 2 ship.
- `Color.Indexed`/`Color.Rgb` work end-to-end with capability downgrade.
- Resize via signal where supported.
- Five-module structure published; aggregator works.
- All existing samples and goldens still pass (with mechanical updates).

---

## 5. Stage 2 — Capability expansion (target: 0.4.0) — **done 2026-04-27**

User-visible features that turn TermFlow from "good for prompts" into "good
for real apps".

**Sequencing.** The two P0 items (mouse, unicode width) are the most visible
and the most invasive. We ordered Stage 2 smallest-first so each landing is
self-contained and the goldens churn least at each step:

| # | Sub-item | Status |
|---|---|---|
| 1 | §5.5 Extended style attributes | done — `Style` gains `italic`/`dim`/`reverse`/`strikethrough`/`blink`; capability gate `Capabilities.extendedStyles` decides emission |
| 2 | §5.6 Extended modifier parsing | done — new `KeyDecoder.Modifiers`, `InputKey.Modified`, `Insert`/`PageUp`/`PageDown`; CSI parser unified to read params then dispatch |
| 3 | §5.4 Bracketed paste | done — `Capabilities.bracketedPaste`, `ANSI.enableBracketedPaste`/`disableBracketedPaste`, `InputKey.Paste(text)` collapses the whole `200~ … 201~` window into one event |
| 4 | §5.3 Unicode width handling | initial cut — `WCWidth` helper, `RenderCell.width`, layout / diff respect wide cells. Known gap: `Prompt`/`InputNode` cursor math is still 1-char-per-column; multi-line input + grapheme clusters deferred |
| 5 | §5.1 Mouse support | done — `MouseEvent` / `MouseButton` / `ScrollDirection` ADTs, `ANSI.enableMouse`/`disableMouse` SGR-1006 + button-event tracking, `InputKey.Mouse(event)` multiplexed onto the key stream so existing `Sub.InputKey` handlers see it. Hit-testing on the layout-pass rect cache is deferred to Stage 3 |

The showcase demo (`sbt showcase`) was extended in the same session to
exercise every sub-item — Styles panel for §5.5, Live-input panel for
§5.6/§5.4/§5.1, Unicode panel for §5.3 — so Stage 2 is end-to-end visible
in one screen. The Themes and Borders panels are click-to-select and
scroll-to-cycle, which is the first user-driven mouse interaction shipped
in the codebase.

### 5.x Bug fix landed alongside Stage 2

- **Overlay background opacity** — `BoxNode` only painted the border, so
  any panel beneath the dialog rectangle bled through the interior.
  `AnsiRenderer` now wipes the overlay's full rectangle with blank cells
  (in `buildFrame`) and emits explicit-space rows (in `renderPatch`)
  before drawing the overlay's children. Two `OverlaySpec` regression
  tests pin the behaviour.


### 5.1 Mouse support (P0, medium)

**Sequences.** Enable xterm SGR-1006 (`CSI ? 1006 h` + `CSI ? 1000 h`) on
startup, disable on shutdown. Parse `CSI < button ; col ; row M/m`.

**Public shape.**

```scala
enum MouseEvent:
  case Press(button: MouseButton, col: Int, row: Int, mods: Modifiers)
  case Release(button: MouseButton, col: Int, row: Int, mods: Modifiers)
  case Drag(button: MouseButton, col: Int, row: Int, mods: Modifiers)
  case Move(col: Int, row: Int)
  case Scroll(direction: ScrollDir, col: Int, row: Int)

object Sub:
  def Mouse[Msg](toMsg: MouseEvent => Msg): Sub[Msg]
```

**Hit testing.** Layout pass annotates each `VNode` with its rect; runtime
maps mouse events to nodes via a hit-test cache. Widgets opt in to mouse via
a `WidgetMouse` typeclass.

**Capability flag.** `Capabilities.mouse: Boolean` — gated on
`xterm`/`xterm-256color`/`screen`/`tmux` and similar.

### 5.2 256-color + truecolor rendering (covered in 4.4)

The model lands in 0.3; renderer end-to-end support and downgrade tables in
0.4.

### 5.3 Unicode width handling (P0, medium)

**Problem.** Today every glyph is assumed width 1. Any CJK character or
emoji breaks layout silently.

**Proposed.** Use JLine's `WCWidth` (or vendor `wcwidth.c` ports) for
column-width calculation. `StyledRun` carries the rendered width; layout and
diff use it.

**Edge cases.** Combining marks (zero width), zero-width joiners (emoji
sequences like 👨‍👩‍👧), East Asian ambiguous width (locale-dependent —
default to narrow, configurable).

**Tests.** Add a `unicode/` sample app exercising CJK, emoji, ZWJ,
combining, RTL.

### 5.4 Bracketed paste (P1, small)

Enable `CSI ? 2004 h` on startup. Parse `ESC [ 200 ~` ... `ESC [ 201 ~`
into a single `KeyEvent.Paste(text: String)`. Currently a long paste
generates N keypresses, racing with rendering.

### 5.5 Extended style attributes (P1, small)

Add `italic`, `reverse`, `dim`, `strikethrough`, `blink` to `Style`.
Capability-gate; downgrade gracefully.

### 5.6 Extended modifier parsing (P1, small)

Today only Ctrl+letter is reliable. Add full xterm modifier parsing
(`CSI 1 ; mod letter` for Shift/Alt arrows, `CSI key ; mod ~` for modified
function keys). Update `KeyDecoder.scala` and `InputKey` enum.

### 5.7 Definition of done for Stage 2

- Mouse demo app: clickable buttons, list selection on click, scroll.
- CJK/emoji content renders correctly in `widgetsDemo`.
- Long paste arrives as one `KeyEvent.Paste`.
- `Style` carries the extended attributes; demo confirms downgrade.
- Capability detection covers mouse and color depth.

---

## 6. Stage 3 — Breadth (target: 0.5.0)

Polishing the catalogue so users don't reinvent common patterns.

### 6.1 Dialog helpers (P1, medium) — **mostly done**

Layered on §4.2. Helpers are pure presentation builders that return an
`Overlay` ready to drop into `RootNode.overlays`; the dialog state lives
in the app's own model. (We deliberately did **not** ship the Lanterna-
style "dialog is a TuiApp returning a Layer" shape — apps drive dialog
state from their existing `update`/`view` loop, which fits the rest of
TermFlow's design and removes a per-dialog adapter trait we'd otherwise
maintain.)

| Helper | Status | Notes |
|---|---|---|
| `Dialogs.message(title, body, choices, …)` | done | Multi-line message + arbitrary action buttons. |
| `Dialogs.confirm(prompt, yesFocused, …)` | done | Yes/no convenience over `message`. |
| `Dialogs.textInput(title, prompt, value, cursor?, prefix?, …)` | done | Owns its own `InputNode`; supports a fixed pinned prefix. |
| `Dialogs.listSelect(title, items, selectedIndex, maxVisible?, render?)` | done | Selection-following viewport scrolling; custom render callback. |
| `Dialogs.waiting(title, body, tick, frames?, cancelLabel?)` | done | Spinner glyph picked from the tick (modulo); optional cancel button. App drives the tick from a `Sub.Every`. |
| `Dialogs.actionList` | not started | Trivially expressed as `listSelect` over `Choice` values; deferred until a real use case appears. |
| `Dialogs.fileDialog` / `directoryDialog` | not started | Need filesystem traversal helpers and async listing for big directories — own PR. |

The five shipped helpers cover the everyday cases. All five are
exercised by `sbt showcase` (`d` = confirm, `i` = textInput, `l` =
listSelect, `w` = waiting). Tests live in `DialogsSpec`.

### 6.2 Additional widgets (P1, medium) — **complete**

| Widget | Status |
|---|---|
| `CheckBox` | done (PR #169) — `☒` / `☐` glyphs with ASCII fallback. |
| `RadioGroup` | done (PR #169) — `◉` / `○` markers; selected and focused are independent. |
| `Tabs` | done (PR #170) — horizontal bar with `[ X ]` active cell; companion `width` + `hitTest`. |
| `LogView` | done (PR #170) — append-only viewer with viewport scrolling; pure helpers (`wrapLine` / `viewport` / `maxScroll`) for hit-test math. |
| `Tree[A]` | done (PR #170) — generic recursive tree via `Tree.Children[A, Id]` type-class; `▾` / `▸` chevrons + per-depth indent; companion `visibleRows` for hit-testing outside the widget. |
| `ComboBox` (closed) | already shipped as `Select` since 0.2. |
| `Autocomplete` (open) | done (PR #171) — `State[A]` holds input + options + selectedIdx; pluggable `matches` predicate; companion `handleKey` returns `(state, picked: Option[A])`. |
| `Menu` / `MenuBar` | done (PR #171) — horizontal title bar + on-demand dropdown; pure `handleKey` dispatcher returns picked `(menuIdx, itemIdx)`; companion `hitTest` maps clicks to title cells or dropdown rows. |
| `Form` builder | done (PR #171) — declarative `Vector[Form.Row]` of (id, label, widget render fn); pairs with `FocusManager` for navigation; per-row `errors: Map[FocusId, String]` annotations. |
| `SplitPane` | done (PR #171) — horizontal / vertical two-pane layout at a configurable ratio; renderer callbacks receive resolved `(at, w, h)`. Drag-to-resize deferred to the mouse hit-test cache follow-up; companion `dividerRect` exposes the drag region for apps that want to wire it manually now. |

### 6.3 Keymap framework (P1, small) — **done**

Single-key `Keymap` was already shipped earlier with the focus / quit /
editing helpers. Stage 3 extended the module with three additional
pieces:

- **`ChordKeymap[Msg]`** — multi-key sequences keyed by
  `Vector[InputKey]`. The companion `step(state, key)` returns one of
  three `ChordResult` outcomes — `Pending` (the user typed a strict
  prefix and should keep going), `Resolved` (the chord completed and
  the bound message should fire), or `NoMatch` (the partial-or-empty
  sequence couldn't lead to a binding, so it resets and the unmatched
  key falls through). Apps hold a single `ChordState` in their model
  and feed each keystroke through `step`.

- **`ModalKeymap[Mode, Msg]`** — different chord keymaps per mode,
  driving the modal-editor metaphor (Vim's normal/insert/visual).
  `step(mode, state, key)` dispatches into the active mode's chord
  table; mode transitions are themselves messages.

- **`KeymapHelp.overlay`** — drop-in modal `Overlay` listing the
  bindings in a `ChordKeymap` as `<chord>  <description>` rows.
  Pairs with `Keymap.renderChord` for human-readable chord rendering
  (`"C-x C-c"`, `"S-Tab"`, `"Up"`, etc.).

The single-key `Keymap` API is unchanged, and `ChordKeymap.fromKeymap`
promotes it for free, so existing apps don't need to migrate.

### 6.4 Testkit module (P1, small)

Promote `termflow.testkit.*` to a published artifact `termflow-testkit`.
Public API:

- `TuiTestDriver[Model, Msg]` — drives an app deterministically, captures
  frames.
- `Golden` — `assertGolden(driver, "name.golden")`, with
  `-Dtermflow.update-goldens=true`.
- `VirtualTerminalBackend` — first-class.
- `KeySim` — `KeySim.type("hello\n")` → seq of `KeyEvent`s.
- `MouseSim` — `MouseSim.click(2, 3)`, `MouseSim.scrollUp(10, 10, 3)`.

This is **TermFlow's unique selling point** — no Java TUI library has it.
Make it discoverable.

### 6.5 Definition of done for Stage 3

- Five dialog helpers ship with sample apps.
- `Form` builder available; sample form replaces the manual form code in
  `forms/FormDemoApp`.
- `Tabs` widget replaces the bespoke layout in `tabs/TabsDemoApp`.
- `termflow-testkit` published as separate artifact; documented.

---

## 7. Stage 4 — Stabilisation (target: 1.0.0)

The lock-in stage. Goal: produce an API we are willing to keep stable for
years.

### 7.1 MiMa for binary compatibility

Add `sbt-mima-plugin`. Configure to check `termflow-terminal`,
`termflow-screen`, `termflow-app`, `termflow-widgets` against the previous
release. Fail CI on incompatible changes; require explicit
`mimaBinaryIssueFilters` for accepted breaks. Lanterna does not enforce this;
we should.

### 7.2 Docs site

GitHub Pages workflow rendering `docs/` via mdBook or a similar Markdown
static site. Sections (mirroring Lanterna's `docs/contents.md`):

1. **Introduction** — what TermFlow is, why pure-FP, when not to use it.
2. **Tutorials** — four progressive walkthroughs:
   - "Hello, World" — minimal app.
   - "Counter" — model + update + view.
   - "Async work" — `Cmd.FCmd`.
   - "Forms and dialogs" — layers, focus, validation.
3. **Guides** — three layer guides:
   - *Direct terminal access* (`termflow-terminal`).
   - *Buffered screen API* (`termflow-screen`).
   - *Application layer* (`termflow-app` + widgets).
4. **Cookbook** — short "how do I…" entries.
5. **Reference** — generated ScalaDoc.
6. **Migration notes** — 0.x → 1.0.

### 7.3 Sample app catalogue expansion

Today: 13 samples. Target: 25, with one app per major feature:

- `mouse/` — basic mouse interaction.
- `unicode/` — CJK + emoji rendering.
- `dialog/` — every dialog helper.
- `tree/` — file-tree explorer.
- `chat/` — already exists; expand to use streaming + scrollback.
- `dashboard/` — multi-pane realtime metrics.
- `wizard/` — multi-step form with back/forward.
- `editor/` — minimal text editor (proves the layout/coords refactor).
- `ssh-shell/` *(if Stage 5 ships)* — telnet/SSH demo.

### 7.4 Killer demo

A fully-functional `llm4s` chat client in <200 lines of TermFlow, used as
the canonical README screenshot. Demonstrates streaming, dialogs, theming,
mouse, and async tool calls.

### 7.5 Definition of done for 1.0

- MiMa passes; published artifacts are stable.
- Docs site live at `https://llm4s.github.io/termflow` (or similar).
- Tutorial ladder complete.
- Sample app count ≥ 20.
- README headline screenshot is the chat client.
- A migration guide from 0.x exists.

---

## 8. Stage 5 — Alternative backends (post-1.0, speculative)

Not on the critical path. Each is an opt-in module.

### 8.1 Virtual terminal backend (already partially present)

Promoted to first-class in Stage 3 (§6.4). Listed here for completeness.

### 8.2 Swing/AWT emulator backend

`termflow-backend-swing`. Opens a desktop window containing a TUI emulator;
the rest of the framework is unchanged. Lanterna's `SwingTerminal` is the
template.

**Why it matters.** Run a TUI in your IDE, in a Docker exec environment
without TTY allocation, or in a CI dashboard. Massive DX win.

**Effort.** Large — a working font-rendered cell grid, blink, selection,
copy-paste, Unicode-correct width.

### 8.3 Telnet backend

`termflow-backend-telnet`. Boilerplate: bind a port, accept connections,
each connection becomes a `TerminalBackend`. Telnet option negotiation
(NAWS for size, ECHO suppression, binary). Small library; medium effort.

**Use case.** Self-hosted admin consoles, MUDs, BBS-style apps, oncall
debug shells. **Not encrypted** — pair with SSH for production.

### 8.4 SSH backend

`termflow-backend-ssh`. Wrap Apache MINA SSHD or sshj, hand the PTY channel
to a `TerminalBackend` adapter. Lanterna doesn't ship this; a Scala
ecosystem niche.

**Effort.** Medium-large; needs key management, auth, connection state.

### 8.5 Web backend (xterm.js over WebSocket)

`termflow-backend-web`. Serve `xterm.js` plus a WebSocket; the WS frames
become the terminal stream. Run a TUI in a browser tab.

**Effort.** Large but cleanly bounded. Dependencies: an HTTP server, an
xterm.js distribution. Probably the most "wow factor" of the bunch.

---

## 9. Lanterna comparison reference

Captured here for grounding. Source: <https://github.com/mabe02/lanterna>,
v3.1.x, plus its `docs/` tree.

### 9.1 Three-layer architecture

| Layer | Lanterna package | TermFlow equivalent | Status |
|---|---|---|---|
| Terminal | `terminal/`, `terminal/ansi`, `terminal/win32`, `terminal/swing`, `terminal/virtual`, `terminal/telnet` | `termflow.tui.TerminalBackend` (one impl) | partial |
| Screen | `screen/` (`TextCharacter`, `TextColor`, diff repaint) | `AnsiRenderer` + `RenderFrame` (internal) | functional, internal-only |
| GUI | `gui2/` (windows, components, layouts, themes, dialogs) | `TuiApp` + widgets | minimal |

Lanterna's docs enforce a reading order: `using-terminal.md` →
`using-screen.md` → `using-gui.md`. Each layer is publicly usable. We should
mirror this in module layout (§4.6) and docs (§7.2).

### 9.2 Backends

| Backend | Lanterna | TermFlow | Stage |
|---|---|---|---|
| ANSI / unix tty | `UnixTerminal`, `ANSITerminal` | `JLineTerminalBackend` | done |
| Cygwin | `CygwinTerminal` | — | not planned |
| Windows native | `terminal/win32` (JNA) | — relies on JLine | TBD |
| Swing emulator | `SwingTerminal` | — | Stage 5 |
| AWT emulator | `AWTTerminal` | — | Stage 5 (combined with Swing) |
| Telnet server | `TelnetTerminal`, `TelnetTerminalServer` | — | Stage 5 |
| Virtual / test | `DefaultVirtualTerminal` | informal in test code | Stage 3 (promote) |
| SSH | — *(not in Lanterna)* | — | Stage 5, opportunity |
| Web (xterm.js) | — *(not in Lanterna)* | — | Stage 5, opportunity |

### 9.3 Components

| Lanterna | TermFlow | Stage to add |
|---|---|---|
| Label | `TextNode` (implicit) | done |
| Button | `Button` | done |
| TextBox | `TextField` (single-line) | done; multi-line in Stage 3 |
| CheckBox / RadioBoxList | `CheckBox` / `RadioGroup` | done (Stage 3 #169) |
| ComboBox | `Select` (closed) / `Autocomplete` (open) | done |
| ActionListBox | `ListView` (close enough) | done |
| Table | `Table` | done |
| ProgressBar | `ProgressBar` | done |
| Spinner (AnimatedLabel) | `Spinner` | done |
| ScrollBar | — | not started |
| Separator | — | trivial; deferred until a real use case |
| Tree | `Tree` | done (Stage 3 #170) |
| SplitPanel | `SplitPane` | done (Stage 3 #171) — drag-resize deferred |
| Panel | `BoxNode` (close enough) | done |
| MenuBar | `MenuBar` | done (Stage 3 #171) |
| StatusBar | `StatusBar` | done — *not in Lanterna* |

### 9.4 Layout managers

| Lanterna | TermFlow | Stage |
|---|---|---|
| LinearLayout | `Column`, `Row` (in `Layout.scala`, build-time) | refactor in Stage 1 |
| GridLayout | — | Stage 1 |
| BorderLayout | — | Stage 1 |
| AbsoluteLayout | implicit (current) | retained as escape hatch |

### 9.5 Dialog helpers

Lanterna ships `MessageDialog`, `TextInputDialog`, `ActionListDialog`,
`ListSelectDialog`, `WaitingDialog`, `FileDialog`, `DirectoryDialog`.
We now ship `message`, `confirm`, `textInput`, `listSelect`, `waiting`
(see §6.1). `FileDialog` / `DirectoryDialog` and the standalone
`actionList` helper remain to land in Stage 3.

### 9.6 Threading model

Lanterna offers `SameTextGUIThread` (caller pumps) or
`SeparateTextGUIThread` (managed). It does not enforce the GUI thread.

TermFlow has a single command-bus thread driving `update`; subscriptions
publish from their own threads but their messages are linearised through
the bus. This model is simpler and is the right call — keep it.

### 9.7 Testability

Lanterna: `DefaultVirtualTerminal` lets you drive headlessly, but no
golden-snapshot framework ships.

TermFlow: `TuiTestDriver` + golden support. **Strict win for us.** Promote
to a published `termflow-testkit` artifact (§6.4).

### 9.8 Documentation

Lanterna: ~10 markdown chapters under `docs/`; tutorials; examples for
every component; partially out-of-date for v3 in places (`GUIGuideDialogs`
banner).

TermFlow: `DESIGN.md`, `RENDER_PIPELINE.md`, `RUN_EXAMPLES.md` — strong
on design rationale, light on user-facing tutorial. Closed in Stage 4
§7.2.

---

## 10. Open questions

1. ~~**Coroutine integration.** Should `Cmd.FCmd` accept anything beyond
   `Future`?~~ **Decided 2026-04-27 — stay stdlib.** TermFlow speaks
   `scala.concurrent.Future` plus `Result[A] = Either[TermFlowError, A]`,
   exposed as `type AsyncResult[+A] = Future[Result[A]]` mirroring the
   `llm4s` core 1:1 so values cross between the two libraries without an
   adapter. `Cmd.asyncResult(task, onSuccess, onError)` is the ergonomic
   bridge. **No** `termflow-effect-cats` / `termflow-effect-zio` modules
   are planned; apps using `IO`/`ZIO` bridge to `Future` at the `Cmd`
   boundary in a couple of characters. Rationale: zero new abstractions,
   composes with `llm4s` and any other stdlib-`Future` library, no
   per-effect-system maintenance tax on every release.
2. **Cross-publish for Scala 2.13.** Already on `legacy-213-track`. Keep
   parity through 1.0, then re-evaluate based on adoption.
3. **Native image.** `sbt-native-image` build for `graalvm-native-image`?
   The shutdown hook + JLine reflection make this non-trivial. Investigate
   in Stage 4 or post-1.0.
4. **Resizing semantics.** When the terminal shrinks, do we clip or rewrap?
   Currently clip. Stage 1's layout pass should make rewrap cheap.
5. **i18n.** Right-to-left text? Bidi? Probably out of scope; document as
   a known limitation.
6. **Accessibility.** Screen readers don't really hit a TUI — they read
   the terminal directly. So our concern is producing terminal output that
   reads cleanly. Worth a "Accessibility notes" docs section.

---

## 11. Decision log

> Append entries here as decisions are made. Each entry: date, decision,
> rationale, alternatives considered.

- *2026-04-26* — Roadmap drafted. Direction: keep pure-FP core, borrow
  Lanterna's layering and breadth, target 1.0 after three breaking 0.x
  refactor stages. Alternatives considered: (a) port Lanterna directly
  (rejected — wrong shape for Scala); (b) freeze 0.2 and call it done
  (rejected — coordinate model and missing layers will become 2.0
  refactors otherwise).
- *2026-04-27* — Stage 1 marked done; Stage 2 begins. The §4.6 module split
  was deliberately reduced to "promote `termflow-testkit`" only; the rest of
  the carve-out (`termflow-terminal/screen/app/widgets`) deferred to Stage 4
  where it pairs naturally with MiMa setup. Stage 2 sequenced
  smallest-first (5.5 → 5.6 → 5.4 → 5.3 → 5.1) to keep golden churn local
  and let each piece ship independently.
- *2026-04-27* — Stage 2 complete. Mouse landed as a single `InputKey.Mouse`
  multiplexed onto the existing key stream rather than a parallel
  `Sub.Mouse` source — this avoids two threads racing for bytes from the
  reader and lets every existing `Sub.InputKey` consumer handle clicks by
  pattern-matching. The roadmap's separate `Sub.Mouse` factory is reserved
  for Stage 3, where it can sit on top of the layout-pass hit-test cache
  and deliver per-widget click messages instead of raw screen coords.
- *2026-04-27* — Showcase rewritten to use absolute panel positioning so the
  Themes / Borders panels can be left-clicked to select and scroll-wheeled
  to cycle. This is the first user-driven mouse interaction in the
  codebase and validates the Stage 2 §5.1 wiring end-to-end. While
  building this we hit and fixed the overlay-opacity bug: dialog interiors
  no longer leak the panels beneath them, because `AnsiRenderer` now wipes
  the overlay rectangle before drawing children.
- *2026-04-27* — Effect-system question closed. `Cmd.FCmd` stays
  `Future`-typed; `AsyncResult[+A] = Future[Result[A]]` ships in
  `TuiPrelude` mirroring `llm4s`, and `Cmd.asyncResult` is the one-liner
  ergonomic bridge for "async work with a typed error". No
  `termflow-effect-cats` / `termflow-effect-zio` modules will ship — the
  cost (extra publish, MiMa, docs, expert maintainer per effect system)
  isn't justified when the escape hatch (`io.unsafeToFuture()`) is two
  characters of glue at the call site.
- *2026-04-27* — Stage 3 §6.1 dialog helpers shipped as
  presentation-only `Overlay` builders rather than the Lanterna-style
  "dialog is a `TuiApp` returning a `Layer`" originally sketched in the
  roadmap. Reason: Lanterna's shape requires a per-dialog adapter trait
  and a result-type plumbing layer that doesn't compose with TermFlow's
  pure `update`/`view` loop. Builders fit the existing architecture, are
  trivially testable in isolation, and let app code stay one Elm-style
  loop end-to-end. Five helpers shipped (`message`, `confirm`,
  `textInput`, `listSelect`, `waiting`); `FileDialog` and the standalone
  `actionList` deferred to a follow-up PR.
- *2026-04-28* — Stage 3 §6.3 keymap framework completed. Single-key
  `Keymap` left untouched; `ChordKeymap[Msg]`, `ModalKeymap[Mode, Msg]`,
  and `KeymapHelp.overlay` added alongside it. Chord dispatch returns
  one of three explicit outcomes (`Pending`/`Resolved`/`NoMatch`) so
  apps can interleave fall-through to other handlers when a key isn't
  part of a chord — the alternative (a single `Option[Msg]` return)
  loses the prefix/no-match distinction the dispatcher needs. Same PR
  migrates `forms/FormDemoApp` to use the `Form.column` builder
  shipped in #171, completing the Stage 3 DoD on that demo.

---

## 12. References

- Lanterna: <https://github.com/mabe02/lanterna>
- Lanterna docs ToC: <https://github.com/mabe02/lanterna/blob/master/docs/contents.md>
- Lanterna `using-terminal.md`, `using-screen.md`, `using-gui.md`
- TermFlow `DESIGN.md`, `RENDER_PIPELINE.md`
- The Elm Architecture: <https://guide.elm-lang.org/architecture/>
- xterm control sequences (CTLSEQS): <https://invisible-island.net/xterm/ctlseqs/ctlseqs.html>
- NO_COLOR spec: <https://no-color.org/>
- WCWidth (Markus Kuhn): <https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c>
