# Capture mouse clicks on a custom widget

The screen layer ships a hit-test cache built into the layout pass.
Wrap any subtree in `Layout.Zone(id, content)` and call
`Layout.resolveTracked[Id]` instead of `resolve` — you get a
`HitTest[Id]` mapping click coordinates back to your zone IDs.

This is how the showcase routes clicks on its action-list dialog,
its tree, and its tab headers.

## Pattern

1. Tag every interactive sub-layout with `Layout.Zone(id, content)`.
   IDs can be any type — `String`, an `enum`, a generic `Int`.
2. Use `Layout.resolveTracked[Id](layout, at, w, h)` when rendering;
   keep the returned `HitTest[Id]` on the model (or rebuild it
   every frame).
3. On a `MouseEvent.Press`, ask the cache `hits.hit(col, row)` and
   dispatch the matching `Msg`.

## Code

```scala
import termflow.tui.{HitTest, Layout, Coord, MouseEvent, KeyDecoder}
import termflow.tui.KeyDecoder.InputKey.*

enum WidgetZone:
  case ButtonA, ButtonB, ListRow(idx: Int)

final case class Model(
  hits:        HitTest[WidgetZone],
  selected:    Option[Int],
  /* … */
)

enum Msg:
  case Mouse(e: MouseEvent)
  case ClickedA
  case ClickedB
  case ClickedRow(idx: Int)

def buildLayout(rowCount: Int): Layout =
  Layout.column(gap = 1)(
    Layout.zone(WidgetZone.ButtonA,
      Layout.Elem(TextNode(1.x, 1.y, List("[ A ]".text(fg = Color.Green))))
    ),
    Layout.zone(WidgetZone.ButtonB,
      Layout.Elem(TextNode(1.x, 1.y, List("[ B ]".text(fg = Color.Cyan))))
    ),
    Layout.column(gap = 0)(
      (0 until rowCount).toList.map(i =>
        Layout.zone(WidgetZone.ListRow(i),
          Layout.Elem(TextNode(1.x, 1.y, List(s"row $i".text)))
        )
      )*
    )
  )

def view(m: Model): RootNode =
  given Theme = Theme.dark
  val (nodes, hits) = Layout.resolveTracked[WidgetZone](
    buildLayout(rowCount = 10),
    at              = Coord(2.x, 2.y),
    availableWidth  = 78,
    availableHeight = 22
  )
  // Stash the hit-test cache on the next render — store it in the model
  // via a Cmd.GCmd or accept the rebuild cost (which is tiny).
  RootNode(80, 24, children = nodes, input = None)

def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Msg.Mouse(MouseEvent.Press(_, col, row, _)) =>
      m.hits.hit(col, row) match
        case Some(WidgetZone.ButtonA)        => m.gCmd(Msg.ClickedA)
        case Some(WidgetZone.ButtonB)        => m.gCmd(Msg.ClickedB)
        case Some(WidgetZone.ListRow(idx))   => m.gCmd(Msg.ClickedRow(idx))
        case None                            => m.tui
    case Msg.Mouse(_)                        => m.tui
    case Msg.ClickedA                        => /* ... */
    case Msg.ClickedB                        => /* ... */
    case Msg.ClickedRow(i)                   => m.copy(selected = Some(i)).tui
```

## Notes

- **The cache is per-frame.** Rebuild it inside `view` (or via a
  helper called from `view`) and either pass it through to a
  click-handler in the same closure, or stash it in the model so
  `update` can look it up next time. The showcase uses the second
  approach — see `actionListHitTest` in
  [`Stage1ShowcaseApp.scala`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/showcase/Stage1ShowcaseApp.scala).
- **Topmost wins.** When zones nest, `hits.hit(col, row)` returns the
  *last-inserted* match — overlays naturally win over the background.
- **`Layout.zone(id, vnode)`** is shorthand for
  `Layout.Zone(id, Layout.Elem(vnode))` when you want to tag a single
  vnode without wrapping it in a layout.
- **No `Zone` for keyboard.** Hit-test is mouse-only. Focus
  navigation routes through `FocusManager`, which uses `FocusId`.
  The two are deliberately separate concerns.

The full hit-test surface lives in
`modules/termflow-screen/src/main/scala/termflow/tui/HitTest.scala`.
