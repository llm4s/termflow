# Full-screen layouts that reflow on resize

If you build your view with `Layout.row` / `Layout.column` / `Layout.Fill`
and want the result to fill the terminal — and *keep* filling it when the
user resizes — you need TermFlow to resolve the layout against the actual
frame size at render time, not when `view` runs.

## The two `RootNode` shapes

| Shape | When the layout resolves | `Layout.Fill` behaviour |
|---|---|---|
| `RootNode(width, height, children, input)` | At `view` time, eagerly | Collapses to natural (zero) size |
| `RootNode(width, height, Nil, input, layout = Some(l))` | At render time, against the frame's `(width, height)` | Expands to fill the available space |

Two helpers on `Layout` produce each shape:

```scala
val eager     = layout.toRootNode(width = w, height = h)              // children = resolve(...)
val budgeted  = layout.toBudgetedRootNode(width = w, height = h)      // layout = Some(layout), children = Nil
```

`toRootNode` is the right tool for fixed-size sub-regions, dialogs, and
golden-snapshot tests where you want deterministic positions. **For
full-screen apps containing `Fill` / `Grid` / `Border`, use
`toBudgetedRootNode`** — that's the only form where the renderer knows
the budget the layout should reflow into.

## Recipe — header / fill / footer

Take the canonical "header on top, scrolling middle, status bar pinned
to the bottom" layout. The middle section uses `Layout.Fill` so it grows
and shrinks with the terminal.

```scala
import termflow.tui.*
import termflow.tui.ScreenPrelude.*
import termflow.tui.Tui.*

case class Model(width: Int, height: Int, log: List[String])

enum Msg:
  case Resize(w: Int, h: Int)
  case Quit

object App extends TuiApp[Model, Msg]:

  def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
    Model(ctx.terminal.width, ctx.terminal.height, log = Nil).tui

  def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
    msg match
      case Msg.Resize(w, h) => m.copy(width = w, height = h).tui
      case Msg.Quit         => Tui(m, Cmd.Exit)

  def view(m: Model): RootNode =
    val header = TextNode(1.x, 1.y, List(" TermFlow demo ".text(bold = true)))
    val body   = TextNode(1.x, 1.y, List(m.log.lastOption.getOrElse("(idle)").text))
    val footer = TextNode(1.x, 1.y, List(s" ${m.width}×${m.height}  q: quit ".text))

    // Fill takes the rest of the column once header / footer are sized.
    // Without toBudgetedRootNode the Fill region would collapse to zero rows.
    val layout = Layout.Column(
      gap = 0,
      children = List(
        header.asLayout,
        Layout.Fill(body.asLayout),
        footer.asLayout
      )
    )
    layout.toBudgetedRootNode(width = m.width, height = m.height)

  def toMsg(input: PromptLine): Result[Msg] = Right(Msg.Quit)
```

Wire `Sub.TerminalResize` so `Msg.Resize` keeps `(width, height)` current
and the renderer's frame budget tracks the live terminal — `Fill` then
absorbs the extra rows automatically. No bespoke "what's the available
height after the header" arithmetic in `view`.

## Why both forms exist

Eager resolution is genuinely useful:

- **Fixed-size widgets and dialogs.** A 40×10 confirm dialog drawn at a
  computed offset doesn't need to know the frame width.
- **Golden-snapshot tests.** Asserting on `RootNode.children` is much
  easier when the children list is populated.
- **Composition.** You can resolve a sub-layout to a list of vnodes, mix
  with hand-positioned nodes, and pass the lot to `RootNode(children =
  ...)`.

The deferred `toBudgetedRootNode` form trades that ergonomics for the
guarantee that `Fill` actually fills and the layout reflows on resize.
For most "the whole TUI is one layout" apps, that's the right trade.

## Mixing both

Both fields on `RootNode` may be set at once. `children` paints first,
then the resolved `layout` is composited on top:

```scala
RootNode(
  width = m.width,
  height = m.height,
  children = staticBackground,         // eager — fixed positions
  input = focusedInput,
  layout = Some(reflowableForeground)  // budget-aware — reflows on resize
)
```

This is the seam most non-trivial apps end up using: a static decor
behind a layout-driven main panel.

## See also

- [Screen layer guide](../guide/screen-layer.md#fill--eat-remaining-space)
  for the full `Layout` DSL.
- [`Layout.Grid` / `Layout.Border`](../guide/screen-layer.md#grid--fixed-column-grid-with-optional-span)
  — both produce real reflow only under `toBudgetedRootNode` (or an
  explicit `RootNode(layout = Some(...))`).
