# Migration notes

> **0.2.0 / 0.3.0 → 1.0 — no migration required.** TermFlow 1.0 is
> source-compatible with 0.2.x / 0.3.x. Apps that compiled and ran
> against either pre-1.0 release should compile and run against 1.0
> unchanged.

The pre-1.0 cycle deliberately froze the public API surface; every
landing in the 0.2.x → 1.0 window has been additive. The list below
catalogues the user-visible additions you can opt into in a 1.0 app
that was originally written against 0.2.0.

## What's new since 0.2.0

### Recoverable error overlay

`Cmd.TermFlowErrorCmd(err: TermFlowError)` raises a transient red
banner above the next frame and clears it on the following render —
the runtime keeps looping. See
[the app-layer guide](../guide/app-layer.md#how-errors-reach-the-user)
for the rendered shape and the testkit hook
(`TuiTestDriver.observedErrors`).

`Cmd.asyncResult(task, onSuccess, onError, onEnqueue)` is the
preferred bridge for fallible async work — it folds an
`AsyncResult[A]` (i.e. `Future[Result[A]]`) into the runtime,
routing `Right(a)` through `onSuccess` and `Left(err)` through
`onError`. `Future` failures themselves still surface via
`TermFlowErrorCmd` automatically.

### Notifications

Two new commands plus matching capability detection:

- `Cmd.RequestAttention` — ring the bell or pop the terminal's
  attention indicator.
- `Cmd.Notify(title, body)` — desktop notification on iTerm2 / kitty /
  GNOME Terminal (VTE), with BEL fallback.

`Capabilities` gained a `notifications: NotificationKind` field
(`Disabled | BellOnly | ITerm2 | Kitty | Vte`). The full pattern is
documented in the
[notifications cookbook](../cookbook/notifications.md).
Override the protocol via `TERMFLOW_NOTIFICATIONS=off|bell|auto`.

### Layout: Grid, Border, budgeted resolve

`Layout.Grid(columns, rowGap, colGap, cells)` and
`Layout.Border(top, left, center, right, bottom, gap)` join the
existing `Row` / `Column` / `Fill` / `Zone` cases. `GridCell`
expresses row / column spans. Both flow through `Layout.resolveTo` so
the renderer reflows them on resize.

`Layout.toBudgetedRootNode(width, height, input)` is the new deferred
sibling to the eager `toRootNode` — it puts the layout into
`RootNode.layout` so the renderer resolves it against the actual
frame budget at render time. This is the right form for full-screen
apps and any layout containing `Fill` / `Grid` / `Border`. See the
[full-screen-layout cookbook](../cookbook/full-screen-layout.md) for
the eager-vs-deferred trade-off.

### Mouse-wheel scrollback

`LogView.scrollDelta(event, viewport, ticksPerDetent)` plus
`LogView.Viewport(at, width, height)` map a `MouseEvent.Scroll` onto
a clamp-friendly scroll delta when the wheel lands inside the
viewport, returning `None` for outside-the-rect or non-scroll events.
Wired into the `chatDemo` sample; covered in the
[streaming-output cookbook](../cookbook/streaming-output.md).

## Post-1.0 — when MiMa lights up

Binary-compatibility enforcement (`sbt-mima-plugin`) is scheduled for
the 1.0.x / 1.1 cycle, with `1.0.0` as the baseline. From the first
post-1.0 release onwards this page will list:

- A summary of breaking changes from the most recent line.
- The `mimaBinaryIssueFilters` rationale for any deliberately accepted
  breaks.
- Code-level before / after recipes for the most common patterns
  affected.
- Deprecation timelines for symbols slated for removal in the next
  major release.

Until then, the [release notes](https://github.com/llm4s/termflow/releases)
remain the source of truth for additive changes.
