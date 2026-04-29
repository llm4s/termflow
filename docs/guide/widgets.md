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

Recursive collapsible tree.

- Expanded glyph: `[-] `
- Collapsed glyph: `[+] `
- Leaf glyph: `    ` (four spaces)

```scala
val tree = Tree.State(root = Tree.Node("root", children = …))
val nodes = Tree.view(tree, width = 32, focused = true)

// Mouse: distinguish chevron clicks from label clicks
Tree.hitTest(rows, at = Rect(...), indentWidth = 2, col, row, labelLength) match
  case Tree.HitResult.Chevron(idx) => /* toggle expansion */
  case Tree.HitResult.Label(idx)   => /* select */
  case _                           => /* miss */
```

### LogView

Tail-following log buffer. Auto-scrolls until the user scrolls up,
then pauses until they scroll back to the bottom.

```scala
val log = LogView.State.empty
val updated = LogView.append(log, "build started", Style(fg = Yellow))
val node = LogView.view(updated, width = 80, height = 16)
```

## Layout

### Tabs

```scala
val state = Tabs.State(labels = Vector("Inputs", "Data", "Layout"), activeIdx = 0)
val (next, _) = Tabs.handleKey[Msg](state, key)(_ => None)
val node = Tabs.view(next, width = 80, focused = true)
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
val menus = Vector(
  MenuBar.Menu("File", items = Vector("Open…", "Save", "Quit")),
  MenuBar.Menu("Edit", items = Vector("Undo", "Redo"))
)
val state = MenuBar.State(menus = menus, openIdx = None)
val node  = MenuBar(state, width = 80)
```

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
