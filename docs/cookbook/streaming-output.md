# Stream output into a scrollback view

`LogView` is a stateless renderer that takes a `Seq[String]` and a
`scrollOffset`. The app owns the buffer and decides whether to
auto-tail (stay pinned to the latest line) or pause (when the user
has scrolled up).

This pattern fits any streaming source: LLM tokens, build output,
log tails, network frames.

## Pattern

1. Hold the buffer (`Vector[String]`) plus a `scrollOffset` and an
   `autoTail: Boolean` flag on the model.
2. As new lines arrive (via `Cmd.GCmd(NewLine(...))` or
   `Sub.Every` polling), append to the buffer; if `autoTail` is set,
   bump `scrollOffset` to keep the bottom in view.
3. ArrowUp / ArrowDown adjust `scrollOffset` and toggle `autoTail`.

## Code

```scala
import termflow.tui.widgets

final case class Model(
  buffer:       Vector[String],
  scrollOffset: Int,
  autoTail:     Boolean
):
  def append(line: String, viewW: Int, viewH: Int): Model =
    val nextBuf  = (buffer :+ line).takeRight(2000)   // bound the history
    val maxScr   = widgets.LogView.maxScroll(nextBuf, viewW, viewH, wrap = true)
    val nextScr  = if autoTail then maxScr else math.min(scrollOffset, maxScr)
    copy(buffer = nextBuf, scrollOffset = nextScr)

enum Msg:
  case TokenArrived(text: String)
  case Up
  case Down

def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Msg.TokenArrived(t) =>
      m.append(t, viewW = ctx.terminal.width - 4, viewH = 16).tui

    case Msg.Up =>
      val maxScr = widgets.LogView.maxScroll(m.buffer, ctx.terminal.width - 4, 16, true)
      m.copy(
        scrollOffset = math.max(0, m.scrollOffset - 1),
        autoTail     = false
      ).tui

    case Msg.Down =>
      val maxScr = widgets.LogView.maxScroll(m.buffer, ctx.terminal.width - 4, 16, true)
      val next   = math.min(maxScr, m.scrollOffset + 1)
      m.copy(scrollOffset = next, autoTail = next == maxScr).tui

def view(m: Model): RootNode =
  given Theme = Theme.dark
  val viewW = 80 - 4
  val nodes = widgets.LogView(
    lines        = m.buffer,
    width        = viewW,
    height       = 16,
    scrollOffset = m.scrollOffset,
    at           = Coord(2.x, 2.y),
    wrap         = true
  )
  RootNode(80, 24, children = nodes, input = None)
```

## Notes

- **Bound the buffer.** `takeRight(2000)` keeps the history finite —
  important when streams can run for hours. The actual cap depends on
  your domain.
- **Token vs line.** If your source emits tokens (LLM-style), keep a
  *partial-line* string on the side and only append to `buffer` when
  you see a `\n`. Otherwise every token becomes its own row.
- **Auto-tail toggles automatically.** When the user scrolls back to
  the bottom (offset == maxScroll), `Down` flips `autoTail = true`
  again — the convention every chat client uses.
- **Dropping `wrap = true`** truncates rather than wrapping; the
  `maxScroll` math still works either way.
- **Mouse-wheel scrollback.** Wire mouse events through
  `LogView.scrollDelta(event, viewport, ticksPerDetent)` — it returns
  `Some(delta)` only when the scroll lands inside the viewport rectangle
  you describe with `LogView.Viewport(at, width, height)`. Reuse the
  same `at` / `width` / `height` you passed to `LogView.apply`, and feed
  the returned delta into the same scroll-update path the keyboard uses.
  Outside-the-viewport events return `None` so a wheel hovering over the
  prompt or the status row won't accidentally page through history.
  Default `ticksPerDetent = 3` matches the speed terminal users expect;
  pass `1` for one-line-per-detent. Keyboard equivalents (↑/↓, PageUp /
  PageDown, End) keep working when mouse reporting is unavailable —
  always wire both.

For an end-to-end example, see
[`apps.echo.EchoApp`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/echo/EchoApp.scala),
which uses a hand-rolled line column instead of `LogView` but
implements the same scrollback semantics — you can swap to `LogView`
in 20 lines.
