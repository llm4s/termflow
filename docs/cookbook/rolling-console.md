# Build a rolling console / agent UI

The Claude Code / Cursor pattern: a transcript scrolls upward as the
agent works, the prompt stays pinned at the bottom, and new output
auto-tails while the user is at the bottom — but pauses if they scroll
back up to read history.

This recipe builds that out of `widgets.LogView` + a bottom-row
`InputNode` (driven by `Prompt`).

## What "rolling" means here

**TermFlow owns its own scrollback viewport inside the alternate
buffer.** When `TuiRuntime.run` starts, it switches the terminal into
the alt buffer; from that point on, the terminal emulator's own
scrollback bar shows nothing useful — your app is in charge of "what's
above the prompt." This recipe is about doing that *well*.

What this is **not**: native terminal scrollback (where output appends
to the terminal's real history and the emulator's scrollbar / copy /
search keep working). That's a deliberately different runtime contract
— there's a sketch under §5.3 of the roadmap for a post-1.0 rolling-
console renderer, but it's not the default.

For 99% of agent / REPL / build-runner UIs the in-app viewport is what
you want, because it gives you total control over how the transcript
re-renders on resize, how auto-tail behaves, and what stays pinned to
the bottom.

## Pattern at a glance

1. Hold an append-only `Vector[String]` buffer (or richer line records),
   plus a `scrollOffset: Int` and an `autoTail: Boolean` flag.
2. As new output arrives (streamed tokens, completed lines, agent
   events): append to the buffer, bound it, and — if `autoTail` is on —
   clamp `scrollOffset` to the live tail.
3. Arrow keys / PageUp / PageDown / mouse wheel adjust `scrollOffset`
   and turn `autoTail` off if the user moves up.
4. `End` (or scrolling back to the bottom) re-enables `autoTail`.
5. Render through `Layout.Border` so the prompt row stays pinned even
   as the terminal resizes.

## Model

```scala
import termflow.tui.*
import termflow.tui.widgets

final case class Model(
  width:        Int,
  height:       Int,
  buffer:       Vector[String],
  scrollOffset: Int,            // display lines from the tail
  autoTail:     Boolean,        // pinned to the bottom
  prompt:       Prompt.State
)

enum Msg:
  case OutputLine(text: String)
  case ScrollBy(delta: Int)
  case ScrollToEnd
  case Submit(text: String)
  case Quit
```

The buffer is `Vector[String]` here; in a real agent UI you'd typically
use `Vector[Entry]` where `Entry` carries role / timestamp / styling
and gets flattened to lines just before rendering.

## Auto-tail logic

Three small helpers keep the scroll state honest. They're pure, so
they're trivial to test.

```scala
val MaxHistory = 5_000   // bound the buffer

def transcriptHeight(m: Model): Int =
  // Border shell: 1 row title, 1 row prompt, 1 row status → 3 reserved.
  math.max(1, m.height - 3)

def maxScroll(m: Model): Int =
  widgets.LogView.maxScroll(m.buffer, m.width, transcriptHeight(m), wrap = true)

def clampedScroll(m: Model, candidate: Int): Int =
  math.max(0, math.min(maxScroll(m), candidate))

def appendLine(m: Model, line: String): Model =
  val nextBuf  = (m.buffer :+ line).takeRight(MaxHistory)
  val nextMax  = widgets.LogView.maxScroll(nextBuf, m.width, transcriptHeight(m), wrap = true)
  val nextScr  = if m.autoTail then nextMax else math.min(m.scrollOffset, nextMax)
  m.copy(buffer = nextBuf, scrollOffset = nextScr)
```

`takeRight(MaxHistory)` is what bounds the retained transcript — pick a
number large enough that scrolling back feels useful but small enough
that an all-day session doesn't grow unbounded. 5–20k lines is typical.

## Update

```scala
def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] = msg match
  case Msg.OutputLine(text) =>
    appendLine(m, text).tui

  case Msg.ScrollBy(delta) =>
    val mx     = maxScroll(m)
    val next   = math.max(0, math.min(mx, m.scrollOffset + delta))
    val tail   = next == 0   // 0 == pinned to live tail
    m.copy(scrollOffset = next, autoTail = tail).tui

  case Msg.ScrollToEnd =>
    m.copy(scrollOffset = 0, autoTail = true).tui

  case Msg.Submit(text) =>
    appendLine(m, s"> $text").tui   // and kick whatever runs the work

  case Msg.Quit =>
    Tui(m, Cmd.Exit)
```

The "auto-tail toggles off when you scroll up, back on when you reach
the bottom" rule lives entirely inside `Msg.ScrollBy`. No flag-setting
ceremony elsewhere.

`scrollOffset = 0` is the canonical "pinned to live tail" position —
that's the convention `LogView` uses.

## Wiring keys

Scrollback / lifecycle keys go straight to `Msg`s; printable keys and
Enter fall through to `Prompt.handleKey`, which owns the input line
and emits `Msg.Submit` on Enter:

```scala
case Msg.ConsoleInputKey(k) =>
  val mapped: Option[Msg] = k match
    case InputKey.ArrowUp   => Some(Msg.ScrollBy(-1))
    case InputKey.ArrowDown => Some(Msg.ScrollBy(+1))
    case InputKey.PageUp    => Some(Msg.ScrollBy(-transcriptHeight(m)))
    case InputKey.PageDown  => Some(Msg.ScrollBy(+transcriptHeight(m)))
    case InputKey.End       => Some(Msg.ScrollToEnd)
    case InputKey.Ctrl('C') => Some(Msg.Quit)
    case _                  => None

  mapped match
    case Some(next) => update(m, next, ctx)
    case None       =>
      // Printable keys + Enter belong to the prompt buffer.
      val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toSubmit)
      // On Enter, maybeCmd is Cmd.GCmd(Msg.Submit(text)); otherwise None.
      Tui(m.copy(prompt = nextPrompt), maybeCmd.getOrElse(Cmd.NoCmd))
```

`ChatStreamApp` (linked below) is the live version of this routing, and
`Prompt.handleKey`'s docstring covers the full contract (Ctrl+C / Ctrl+D
emit `Cmd.Exit`, Enter clears the buffer + dispatches the parsed
message, etc.).

## Mouse-wheel scrollback

`LogView.scrollDelta` does the rectangle test for you:

```scala
case Msg.MouseEvent(ev) =>
  val viewport = widgets.LogView.Viewport(
    at     = Coord(1.x, 2.y),
    width  = m.width,
    height = transcriptHeight(m)
  )
  widgets.LogView.scrollDelta(ev, viewport) match
    case Some(d) => update(m, Msg.ScrollBy(d), ctx)
    case None    => m.tui   // wheel was over the prompt or status — ignore
```

Defaults to 3 lines per detent. Wheel events outside the transcript
rectangle (e.g. over the prompt) are ignored, which is what you want.

## View

The transcript renders as a list of `VNode`s from `LogView`; the prompt
goes into the `InputNode` slot on `RootNode` so the runtime knows where
to place the hardware cursor.

```scala
import termflow.tui.*
import termflow.tui.TuiPrelude.*    // brings in the 1.x / "string".text helpers

def view(m: Model): RootNode =
  given Theme = Theme.dark

  val title = TextNode(
    1.x, 1.y,
    List(s" termflow-agent · ${m.buffer.size} lines ".text(fg = Theme.dark.primary))
  )

  val statusLabel = if m.autoTail then "auto-tail"
                   else s"paused @ ${m.scrollOffset} — End to tail"
  val statusRow = TextNode(
    1.x, (m.height - 1).y,
    List(statusLabel.text(fg = Theme.dark.secondary))
  )

  val transcript: List[VNode] = widgets.LogView(
    lines        = m.buffer,
    width        = m.width,
    height       = transcriptHeight(m),
    scrollOffset = m.scrollOffset,
    at           = Coord(1.x, 2.y),     // below the title row
    wrap         = true
  )

  val rendered = Prompt.renderWithPrefix(m.prompt, "> ")

  RootNode(
    width    = m.width,
    height   = m.height,
    children = title :: statusRow :: transcript,
    input = Some(InputNode(
      x            = 1.x,
      y            = m.height.y,        // last row
      text         = rendered.text,
      style        = Style(fg = Theme.dark.success),
      cursor       = rendered.cursorIndex,
      lineWidth    = math.max(1, m.width - 1),
      prefixLength = rendered.prefixLength
    ))
  )
```

The `Prompt` lives in the `InputNode` slot rather than as a regular
`VNode`, so the runtime keeps the cursor on the input line and lets
horizontal scrolling kick in if you type past the visible width.

If you'd rather express the title / transcript / prompt as a structured
layout (and let the screen layer reflow on resize), use
`Layout.border(top = …, center = …, bottom = …).toBudgetedRootNode(width,
height, input = Some(promptInput))` — the
[full-screen layout recipe](full-screen-layout.md) walks the eager-vs-
deferred trade-off in detail.

## Worked example

`ChatStreamApp` (`modules/termflow-sample/.../apps/chat/ChatStreamApp.scala`)
is the live version of every pattern on this page: token-by-token
streaming via `Sub.Every`, auto-tail / pause / resume on `End`,
mouse-wheel scrollback, `Ctrl+L` to clear, `Layout.Border` shell.
Run it with:

```bash
sbt chatDemo
```

If you're building an agent UI, that's the closest thing to a starter
template TermFlow ships.

## Native terminal scrollback?

Sometimes you genuinely want the terminal emulator's own scrollback —
the user's existing `Cmd-K` clear, `Cmd-F` search, copy-paste, and
shell history all keep working. That's a different runtime model: the
app appends to the normal buffer rather than painting fixed frames in
the alt buffer.

It's tracked under §5.3 (post-1.0) on the roadmap as a constrained
`RollingConsoleApp` / renderer. The 1.0 contract is the in-app
viewport pattern this recipe describes; if and when the rolling
renderer ships, recipes here will be updated.
