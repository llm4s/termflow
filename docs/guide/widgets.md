# Widgets

`termflow-widgets` ships a catalogue of reusable components. Every
widget is built on top of `termflow-screen` and `termflow-app`,
follows the same `(State, handleKey, view)` shape, and takes a
`given Theme` so it picks up your colour scheme.

```scala
libraryDependencies += "org.llm4s" %% "termflow-widgets" % "0.2.0"
```

The umbrella `termflow` artefact already depends on this module —
most apps don't need to depend on it directly.

> Every widget is exercised by the
> [showcase app](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/showcase/Stage1ShowcaseApp.scala).
> Run `sbt showcase` to see them rendered side-by-side.

## The widget protocol

Most widgets follow a three-piece pattern:

1. **`State`** — the widget's data (cursor positions, scroll offsets,
   selected index). You store this on your model.
2. **`handleKey(state, key)(...)`** — pure function from a key to the
   next state, optionally producing a `Msg` (e.g. on Enter).
3. **`view(state, …, focused: Boolean = true)`** — pure function from
   state to `VNode` or `List[VNode]`. The `focused` flag controls
   which highlight to use.

Stateless widgets (`Button`, `RadioGroup`, `ProgressBar`, `Spinner`,
`StatusBar`, `Separator`) skip step 2 — they only render.

## Inputs

### TextField

Single-line text input. Handles cursor, paste, grapheme-aware
backspace.

```scala
import termflow.tui.widgets.TextField

val initial = TextField.State.withPlaceholder("alice@example.com")
val (next, _) = TextField.handleKey[Msg](initial, key)(_ => None)
val node = TextField.view(next, lineWidth = 28, focused = true)
```

Placeholder text renders dim+italic until the user types.

### MultiLineInput

Multi-line editor. Cursor row uses reverse-video (TextField-style) so
it embeds cleanly in any layout. Tab inserts a literal `\t`.
Grapheme-aware navigation across all four arrows, Backspace, Delete.

```scala
val state = MultiLineInput.State(lines = Vector("hello", "world"))
val (next, _) = MultiLineInput.handleKey[Msg](state, key)(_ => None)
val node = MultiLineInput.view(next, width = 60, height = 12, focused = true)
```

### Button

Inline `[ Label ]`. Focus = primary background bar. Stateless —
`Button(label, focused)` returns a `VNode` directly.

```scala
widgets.Button(label = "Submit", focused = focusManager.isFocused(SubmitId))
```

### CheckBox / RadioGroup

```scala
widgets.CheckBox(label = "Remember me", checked = true, focused = true)

widgets.RadioGroup(
  options       = Vector("Free", "Pro", "Enterprise"),
  selectedIndex = 1,
  focusedIndex  = 1,
  at            = Coord(4.x, 8.y)
)
```

`RadioGroup` returns `List[VNode]`, one per option. Use `Layout.Column`
or absolute coordinates if you want different placement.

Capability-aware glyphs: ☐/☒ and ◯/◉ on Unicode terminals,
`[ ]/[x]` and `( )/(*)` on ASCII-only.

### Select / Autocomplete

`Select` is a closed-state dropdown — single click opens, click again
or pick an item closes. `Autocomplete` is the open-state variant: a
filterable list always visible underneath an input.

```scala
val acState = Autocomplete.State.of(Vector("apple", "banana", "cherry"))
val r       = Autocomplete.handleKey(acState, key)
val nodes   = Autocomplete.view(r.state, width = 16, maxVisible = 6, focused = true)
```

Both clamp `selectedIdx` into the visible filtered range. The
viewport scrolls so the cursor row stays visible.

### Prompt

Strictly speaking `Prompt` is in `termflow-app`, not `widgets`, but
it's the workhorse for REPL-style apps — see the
[Counter tutorial](../tut/02-counter.md) for the full integration.

## Data display

### ListView

Scrollable, selectable list. `▸ ` cursor when focused, just colour
when blurred.

```scala
val state = ListView.State(items = Vector("apple", "banana", "cherry"))
val (next, _) = ListView.handleKey[Msg](state, key)(_ => None)
val node = ListView.view(next, width = 24, maxVisible = 8, focused = true)
```

### Table

Selectable rows + columns with `Align.Left | Right | Center`.

```scala
val cols = Vector(
  Table.Column("Name",  width = 20, align = Align.Left),
  Table.Column("Score", width = 8,  align = Align.Right)
)
val state = Table.State(columns = cols, rows = Vector(Vector("alice","42")))
```

### Tree

Recursive collapsible tree. Stateless — the app owns `expanded:
Set[Id]` and `selectedIndex: Int`; the renderer takes a typeclass
`Children[A, Id]` describing how to traverse your data structure.

- Expanded glyph: `[-] `
- Collapsed glyph: `[+] `
- Leaf glyph: `    ` (four spaces)

```scala
case class Node(name: String, kids: Vector[Node])
given widgets.Tree.Children[Node, String] with
  def id(n: Node):   String        = n.name
  def kids(n: Node): Vector[Node]  = n.kids

val nodes = widgets.Tree(
  roots         = Vector(Node("src", Vector(Node("Main.scala", Vector.empty)))),
  expanded      = Set("src"),
  selectedIndex = 0,
  render        = _.name,
  at            = Coord(2.x, 2.y)
)

// Mouse: distinguish chevron clicks from label clicks
val rows = widgets.Tree.visibleRows(roots, expanded)
widgets.Tree.hitTest(rows, at = Coord(2.x, 2.y), indentWidth = 2, col, row) match
  case Some(widgets.Tree.HitResult.Chevron(idx)) => /* toggle expansion */
  case Some(widgets.Tree.HitResult.Label(idx))   => /* select */
  case None                                      => /* miss */
```

### LogView

Stateless line-buffer viewer. The app owns the `Vector[String]`
buffer and the current `scrollOffset`; `LogView` wraps and clips
into the requested viewport.

```scala
val node = widgets.LogView(
  lines        = m.logBuffer,            // Seq[String]
  width        = 80,
  height       = 16,
  scrollOffset = m.scrollOffset,
  wrap         = true
)
```

Use `LogView.maxScroll(lines, width, height, wrap)` when the user
scrolls so you clamp the offset correctly.

## Layout

### Tabs

Stateless tab-header renderer — the app owns the active and focused
indices and handles tab-switching keys itself.

```scala
val node = widgets.Tabs(
  labels       = Seq("Inputs", "Data", "Layout"),
  activeIndex  = m.activeTab,
  focusedIndex = if m.headerFocused then m.activeTab else -1,
  at           = Coord(2.x, 1.y),
  separator    = " │ "
)
```

### SplitPane

Horizontal/vertical pane divider. Resize via mouse drag (Stage 3 §6.2)
or via keyboard (`[` / `]`).

```scala
val ds = SplitPane.DragState(splitRatio = 0.5, dragging = false)
val ds2 = SplitPane.handleMouse(
  state = ds, event = mouseEvent,
  direction = SplitPane.Vertical, width = 80, height = 24,
  at = Coord(1.x, 1.y), gap = 1
)
```

### Separator

```scala
widgets.Separator.horizontal(width = 60, at = Coord(1.x, 5.y), title = Some("Section"))
widgets.Separator.vertical(height = 12, at = Coord(40.x, 1.y))
```

### ScrollBar

Visual thumb track. Use alongside `ListView`, `Table`, or
`MultiLineInput` when content exceeds the visible area.

```scala
val sb = ScrollBar.State(offset = 0, visible = 12, total = 60)
if sb.needed then
  val node = ScrollBar(sb, at = Coord(80.x, 2.y), height = 12)
```

## Feedback

### ProgressBar

```scala
widgets.ProgressBar(at = Coord(2.x, 5.y), width = 40, fraction = 0.6)
```

`█` filled, `░` empty on Unicode terminals; `#` / `-` on ASCII.

### Spinner

```scala
val frames = Spinner.Braille  // or .Line, .Dots
val frame  = Spinner.frame(frames, tickIndex)
```

Stateless — you advance `tickIndex` on each `Sub.Every` tick. See the
[async tutorial](../tut/03-async.md) for the full pattern.

### StatusBar

3-column header/footer in inverse video.

```scala
widgets.StatusBar(
  width  = 80,
  left   = " ▌ TermFlow ",
  center = " connected ",
  right  = "  q quit "
)
```

### MenuBar

Top-of-screen menu bar with dropdown items.

```scala
val state = widgets.MenuBar.State(
  menus = Vector(
    widgets.MenuBar.Menu("File", items = Vector("Open…", "Save", "Quit")),
    widgets.MenuBar.Menu("Edit", items = Vector("Undo", "Redo"))
  )
)
val widgets.MenuBar.KeyResult(next, picked) = widgets.MenuBar.handleKey(state, key)
val node = widgets.MenuBar(next, at = Coord(1.x, 1.y), focused = true)
```

`KeyResult.picked: Option[(menuIdx, itemIdx)]` is `Some(...)` only on
the keystroke that committed a selection — fold it into your domain
`Msg` and dispatch.

## Form

The composite `Form.column` helper renders multi-row forms with
labels, focus-aware widgets, and inline validation. The
[forms tutorial](../tut/04-forms-and-dialogs.md#8-building-forms-with-widgetsformcolumn)
walks through it end-to-end.

```scala
widgets.Form.column(
  rows         = Vector(
    Form.Row(NameId,  "Name:",  focused => TextField.view(name,  lineWidth = 28, focused = focused)),
    Form.Row(EmailId, "Email:", focused => TextField.view(email, lineWidth = 28, focused = focused))
  ),
  focusManager = fm,
  at           = Coord(2.x, 4.y),
  labelWidth   = 8,
  gap          = 1,
  errors       = Map("wiz-email" -> "Email must contain '@'")
)
```

## Coverage

The complete file list is checked by
[`scripts/check_widget_docs.sh`](https://github.com/llm4s/termflow/blob/main/scripts/check_widget_docs.sh)
in CI: any new file under
`modules/termflow-widgets/src/main/scala/termflow/tui/widgets/` that
isn't mentioned in this guide will fail the build.

For full per-widget API, see the [Scaladoc](../reference/api.md).
