# Screen layer

`termflow-screen` sits one layer above the TTY. It gives you:

- a **virtual DOM** (`VNode` ADT — `TextNode`, `BoxNode`, `InputNode`,
  `RootNode`),
- a **layout DSL** for stacking, gaps, fills, and zones,
- an **ANSI renderer** that diff-paints frames into minimal terminal
  patches,
- a **hit-test cache** for routing mouse events to logical zones,
- a **theme** model with semantic colour slots and box-drawing chars.

```scala
libraryDependencies += "org.llm4s" %% "termflow-screen" % "0.2.0"
```

You reach for the screen layer directly when you want a draw surface
and rendering, but not the Elm-style runtime — for example, a one-shot
report renderer that emits ANSI to stdout and exits.

## VNode — the virtual DOM

```scala
enum VNode:
  case TextNode(x: XCoord, y: YCoord, txt: List[Text])
  case BoxNode(
    x: XCoord, y: YCoord, width: Int, height: Int,
    children: List[VNode], style: Style, chars: BorderChars
  )
  case InputNode(
    x: XCoord, y: YCoord, prompt: String, style: Style,
    cursor: Int = -1, lineWidth: Int = 0, prefixLength: Int = 0
  )
```

Plus the wrapper:

```scala
final case class RootNode(
  width:    Int,
  height:   Int,
  children: List[VNode],
  input:    Option[InputNode],
  overlays: List[Overlay] = Nil,
  layout:   Option[Layout] = None
)
```

Every render produces a `RootNode`. `AnsiRenderer.render(prev, next)`
diffs the two and emits the ANSI patch.

A few invariants:

- **Coordinates are 1-based.** `XCoord` and `YCoord` are opaque types
  over `Int` so you can't accidentally swap them. Use the `.x` / `.y`
  extension methods in `ScreenPrelude` (`2.x`, `3.y`) to construct.
- **`BoxNode` is visual-only.** It draws a border but does not
  position its children. If you want layout, use `Layout.Column` or
  `Layout.Row` instead and attach the box as a sibling.
- **Only the topmost `InputNode` is live.** It carries the hardware
  cursor position. Nested input nodes are drawn but inert.

> File: `modules/termflow-screen/src/main/scala/termflow/tui/vdom.scala`.

## Layout DSL

`Layout` describes the stacking shape. Build a tree, then `resolve`
it at an origin to flatten it into absolute-coordinate `VNode`s.

```scala
enum Layout:
  case Elem(vnode: VNode)
  case Row(gap: Int, children: List[Layout])
  case Column(gap: Int, children: List[Layout])
  case Spacer(width: Int, height: Int)
  case Fill(content: Layout)
  case Zone(id: Any, content: Layout)
  case Grid(columns: Int, rowGap: Int, colGap: Int, cells: List[GridCell])
  case Border(top: Option[Layout], left: Option[Layout],
              center: Option[Layout], right: Option[Layout],
              bottom: Option[Layout], gap: Int)
```

Concrete usage:

```scala
import termflow.tui.{Layout, BoxNode, TextNode, Style}
import termflow.tui.TuiPrelude.*

val column = Layout.Column(gap = 1, children = List(
  Layout.Elem(TextNode(1.x, 1.y, List("Counter".text(fg = Yellow)))),
  Layout.Elem(TextNode(1.x, 1.y, List(s"value: $count".text))),
  Layout.Spacer(1, 1),
  Layout.Elem(TextNode(1.x, 1.y, List("press + / -".text)))
))

val children: List[VNode] = column.resolve(Coord(2.x, 2.y))
```

Inside the layout tree the inner `(1.x, 1.y)` are *layout-local* —
`resolve` adds the origin to produce the right absolute coordinates.

The fluent factories `Layout.row(gap)(vnode1, vnode2, …)` and
`Layout.column(gap)(...)` skip the `Elem` wrappers when you have a
flat list of vnodes.

### `Fill` — eat remaining space

`Fill(content)` consumes whatever's left on the major axis. Useful
for splits and panels:

```scala
Layout.Row(gap = 1, children = List(
  Layout.Elem(sidebar),    // natural width
  Layout.Fill(mainPanel)   // takes the rest of the row
))
```

Resolve `Fill` regions with `Layout.resolveTo(layout, at, w, h)` — the
form that knows the available width / height.

### `Grid` — fixed-column grid with optional span

Cells flow left-to-right, top-to-bottom. Column widths split the
available width evenly (with a budget) or fall back to per-column
natural widths. `colSpan` / `rowSpan` reserve a rectangle of slots and
the cursor skips past them.

```scala
Layout.grid(columns = 3, rowGap = 1, colGap = 2)(
  TextNode(1.x, 1.y, List("a".text)),
  TextNode(1.x, 1.y, List("b".text)),
  TextNode(1.x, 1.y, List("c".text))
)

// With spans:
Layout.Grid(columns = 2, rowGap = 0, colGap = 0, cells = List(
  GridCell(Layout.Elem(headerNode), colSpan = 2),
  GridCell(Layout.Elem(leftNode)),
  GridCell(Layout.Elem(rightNode))
))
```

Cells with `colSpan > 1` cover the spanned columns inside `resolveTo` /
`resolveTracked`; their content can itself be a `Zone` so mouse clicks
land back on a logical id.

### `Border` — five-zone layout

```scala
Layout.border(
  top    = Layout.Elem(headerNode),
  left   = Layout.Elem(sidebar),
  center = Layout.Elem(mainPanel),
  right  = Layout.Elem(detailsPanel),
  bottom = Layout.Elem(statusBar),
  gap    = 1
)
```

Sizing under a budget:

- **Top / bottom** — natural height, full width.
- **Left / right** — natural width, middle-band height.
- **Center** — fills the remainder.

Pass `null` (or omit the named argument) for any zone you don't need;
its space collapses. Designed for "header / sidebar / main / footer"
shells where you don't want to compute the band heights by hand.

### `Zone` — tag for hit-test

Wrap a sub-layout in `Zone(id, content)` and `Layout.resolveTracked`
will register the resulting rectangle in a `HitTest[Id]` so you can
route mouse clicks back to the same id:

```scala
val (vnodes, hits) = Layout.resolveTracked[String](
  layout, at = Coord(1.x, 1.y), availableWidth = 80, availableHeight = 24
)

mouseEvent match
  case Mouse(MouseEvent.Press(_, col, row, _)) =>
    hits.hit(col, row).foreach(handleZone)
```

> File: `modules/termflow-screen/src/main/scala/termflow/tui/Layout.scala`.

## HitTest

```scala
final case class Rect(x: Int, y: Int, width: Int, height: Int):
  def right:  Int
  def bottom: Int
  def contains(col: Int, row: Int): Boolean
  def at: Coord

final case class HitTest[Id](zones: Vector[(Id, Rect)]):
  def add(id: Id, rect: Rect): HitTest[Id]
  def ++(other: HitTest[Id]): HitTest[Id]
  def hit(col: Int, row: Int): Option[Id]
```

`hit(col, row)` returns the *topmost* zone (last-inserted wins) at the
given coordinate. The cache is built during the layout pass — you
never have to maintain a parallel zone list by hand.

> File: `modules/termflow-screen/src/main/scala/termflow/tui/HitTest.scala`.

## Theme — semantic colour slots

```scala
final case class Theme(
  primary:    Color,  secondary:  Color,
  error:      Color,  warning:    Color,
  success:    Color,  info:       Color,
  border:     Color,  background: Color,  foreground: Color,
  chars: BorderChars = BorderChars.sharp
)

object Theme:
  val dark:    Theme
  val light:   Theme
  val mono:    Theme
  val rounded: Theme
```

A widget that takes `(using Theme)` can draw against the active
theme without knowing anything about the user's palette. Override by
making your own `given Theme = …` in scope, or by passing a theme
explicitly to dialog/widget builders.

`BorderChars` controls box-drawing glyphs:

```scala
object BorderChars:
  val sharp:   BorderChars  // ┌─┐│└┘
  val rounded: BorderChars  // ╭─╮│╰╯
  val double:  BorderChars  // ╔═╗║╚╝
  val ascii:   BorderChars  // +-+|-+ — for terminals without unicode
```

Capability-aware: when `Capabilities.unicode` is false, the renderer
substitutes `ascii` automatically.

> Files: `Theme.scala`, `BorderChars.scala`.

## RenderFrame and AnsiRenderer

The renderer turns a `RootNode` into a `RenderFrame` (a
`width × height` grid of `RenderCell(ch, style, width)`), then diffs
against the previous frame and emits ANSI.

```scala
final case class RenderCell(ch: Char, style: Style, width: Int = 1)

final case class RenderFrame(
  width:  Int,
  height: Int,
  cells:  Array[Array[RenderCell]],
  cursor: Option[Coord]
)
```

You normally don't construct a `RenderFrame` by hand — the runtime
does it for you. Where it shows up is in tests: `TuiTestDriver.frame`
returns the `RenderFrame` produced by the last `view` call, and
`GoldenSupport.assertGoldenFrame` compares it to a snapshot.

The wide-cell handling is critical:
`RenderCell.width = 2` means the next cell is consumed by the same
glyph. This is what makes `好` render correctly.

> File: `modules/termflow-screen/src/main/scala/termflow/tui/AnsiRenderer.scala`.

## ScreenPrelude — sugar

```scala
import termflow.tui.TuiPrelude.*  // lifts ScreenPrelude transitively

2.x                                  // XCoord
3.y                                  // YCoord
"hello".text                         // Text(value, Style.empty)
"warning".text(fg = Color.Yellow)    // styled text
"OK".text(fg, bg, bold = true)       // multiple style args
```

These are the conversions every example uses. Always import
`TuiPrelude.*` at the top of view code; you'll thank yourself.

## When to climb up

The screen layer plus your own loop is enough for read-only tools
(report renderers, status dashboards, pipe viewers). The moment you
want input — keystrokes, timers, async work — you want the
[Application layer](app-layer.md). Everything in this layer is reused
verbatim by the app layer, so nothing is wasted.

The full per-type API is in the [Scaladoc](../reference/api.md).
