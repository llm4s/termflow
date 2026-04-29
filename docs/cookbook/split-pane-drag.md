# Two-pane layout with a draggable splitter

`SplitPane` ships with a `DragState` and a pure `handleMouse` that
turns a `MouseEvent` stream into a new `splitRatio`. Pair it with
keyboard shortcuts (`[` / `]`) and you get a fully interactive
divider in ~20 lines.

## Pattern

1. Hold a `SplitPane.DragState` on the model — `splitRatio: Double`
   in `[0.0, 1.0]`, `dragging: Boolean` for "we're mid-gesture".
2. Route mouse events into `SplitPane.handleMouse` to update drag
   state.
3. Map `[` / `]` (or whatever keys you like) to `splitRatio - 0.05`
   and `+0.05` for keyboard control.
4. Use `m.split.splitRatio` when laying out the two panes.

## Code

```scala
import termflow.tui.widgets.SplitPane
import termflow.tui.{Coord, KeyDecoder, MouseEvent}
import termflow.tui.KeyDecoder.InputKey.*

final case class Model(
  split:    SplitPane.DragState,
  /* … */
)

enum Msg:
  case Mouse(e: MouseEvent)
  case Wider, Narrower, ResetSplit

def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Msg.Mouse(e) =>
      val next = SplitPane.handleMouse(
        state     = m.split,
        event     = e,
        direction = SplitPane.Direction.Vertical,  // pane divider runs vertically
        width     = ctx.terminal.width,
        height    = ctx.terminal.height,
        at        = Coord(1.x, 1.y),
        gap       = 1
      )
      m.copy(split = next).tui

    case Msg.Wider =>
      m.copy(split = m.split.copy(splitRatio = math.min(0.9, m.split.splitRatio + 0.05))).tui

    case Msg.Narrower =>
      m.copy(split = m.split.copy(splitRatio = math.max(0.1, m.split.splitRatio - 0.05))).tui

    case Msg.ResetSplit =>
      m.copy(split = SplitPane.DragState(splitRatio = 0.5, dragging = false)).tui

def view(m: Model): RootNode =
  given Theme = Theme.dark
  val w        = 80
  val h        = 24
  val leftCols = math.max(1, (w * m.split.splitRatio).toInt)

  val left  = Layout.column(gap = 0)(
    /* left-pane vnodes */
  )
  val right = Layout.column(gap = 0)(
    /* right-pane vnodes */
  )

  val divider = TextNode((leftCols + 1).x, 1.y, List(
    "│".text(fg = if m.split.dragging then Color.Yellow else Color.White)
  ))

  val nodes = left.resolve(Coord(1.x, 1.y)) ++
              right.resolve(Coord((leftCols + 2).x, 1.y)) ++
              List(divider)

  RootNode(w, h, children = nodes, input = None)
```

## Notes

- **`Direction.Vertical` means a vertical divider** that splits the
  width — left pane / right pane. `Direction.Horizontal` is a
  horizontal divider (top / bottom split).
- **Recolouring during drag** — `m.split.dragging` is `true` between
  Press and Release; rendering the divider in `theme.warning` gives
  immediate feedback that the gesture is live.
- **Clamp the ratio** to `[0.05, 0.95]` if you want to keep both
  panes visible. `SplitPane.handleMouse` doesn't clamp — it lets you
  drag the divider all the way to the edge.
- **Keystroke vs mouse** — pick reasonable shortcuts for users who
  don't have a mouse or are working over an SSH session that
  doesn't pass mouse events. The showcase uses `[` / `]` / `=`.

For a working example, see the showcase's *Layout* tab —
[`Stage1ShowcaseApp.scala`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/showcase/Stage1ShowcaseApp.scala).
