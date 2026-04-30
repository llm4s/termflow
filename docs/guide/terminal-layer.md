# Terminal layer

`termflow-terminal` is the lowest layer — direct access to the TTY,
key decoding, capability detection, Unicode width and grapheme math.
Most apps never need to touch this module directly: the `app` layer
takes care of opening the terminal, registering input subscriptions,
and switching to the alternate buffer. You reach for the terminal
layer when those abstractions are in the way.

```scala
libraryDependencies += "org.llm4s" %% "termflow-terminal" % "0.2.0"
```

## When to use it directly

- Building a CLI utility that needs raw key reads but no full-screen
  rendering.
- Detecting whether the user's terminal supports true colour, mouse,
  bracketed paste before deciding which features to enable.
- Implementing a custom backend (telnet server, browser bridge) that
  feeds an upper-layer app.
- Writing tests for grapheme- or width-sensitive code without spinning
  up the runtime.

## TerminalBackend

`TerminalBackend` is the trait every terminal implementation
satisfies. It bundles a `Reader` / `Writer` pair, current size, ANSI
emission, and capability detection.

```scala
trait TerminalBackend extends TerminalInfo:
  def reader: Reader
  def writer: Writer
  def width: Int
  def height: Int
  def write(text: String): Unit
  def flush(): Unit
  def close(): Unit
  def capabilities: Capabilities
  def onResize(listener: () => Unit): Option[() => Unit]
```

The default implementation is `JLineTerminalBackend`, backed by
[JLine](https://github.com/jline/jline3). It honours SIGWINCH, so
`onResize` fires the moment the user resizes their window — no
polling.

> File: `modules/termflow-terminal/src/main/scala/termflow/tui/TerminalBackend.scala`.

## KeyDecoder.InputKey

The decoded keystroke ADT. Pattern-match on it to react to user
input:

```scala
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.KeyDecoder.InputKey.*

key match
  case CharKey(c)             => /* printable */
  case Ctrl(c)                => /* control modifier */
  case Enter | Tab | Escape   => /* named keys */
  case ArrowUp | ArrowDown    => /* navigation */
  case Modified(inner, mods)  => /* shift/alt/ctrl/meta combinations */
  case Mouse(event)           => /* SGR-1006 mouse events */
  case Paste(text)            => /* bracketed-paste payload */
  case _                      => /* F1–F12, BackTab, Insert, Home, End, PageUp/Down, NoOp, EndOfInput, Unknown */
```

`Modifiers(shift, alt, ctrl, meta)` decodes xterm's modifier byte; use
`Modifiers.fromXtermCode(code)` if you're decoding raw CSI parameters.

> File: `modules/termflow-terminal/src/main/scala/termflow/tui/KeyDecoder.scala`.

## Capabilities

A small struct describing what the terminal can do, with conservative
defaults so apps that ignore it still work:

```scala
final case class Capabilities(
  colorDepth:     ColorDepth, // Mono | Ansi8 | Ansi16 | Indexed256 | Truecolor
  unicode:        Boolean,
  mouse:          Boolean,
  extendedStyles: Boolean = true,    // italic / dim / strike / blink
  bracketedPaste: Boolean = true,
  notifications:  NotificationKind = NotificationKind.BellOnly
)

object Capabilities:
  def detect(env: Map[String, String]): Capabilities
```

`notifications` controls how `Cmd.RequestAttention` and `Cmd.Notify`
reach the user — values: `Disabled`, `BellOnly`, `ITerm2`, `Kitty`,
`Vte`. The runtime resolves the right OSC sequence per kind; see the
[notifications cookbook](../cookbook/notifications.md).

`Capabilities.detect(sys.env)` honours `NO_COLOR`, `COLORTERM`, `TERM`,
and `LANG` / `LC_*`. The `AnsiRenderer` calls this for you and
downgrades colour emission accordingly — true-colour `Rgb(...)` styles
become indexed-256 or 16-colour ANSI as needed.

`ColorDepth` is monotonic: `td.supports(other)` returns `true` when
`td`'s depth is at least `other`'s. Use it to gate features:

```scala
if caps.colorDepth.supports(ColorDepth.Truecolor) then highColourTheme
else                                                  ansi8FallbackTheme
```

> File: `modules/termflow-terminal/src/main/scala/termflow/tui/Capabilities.scala`.

## Mouse events

Mouse events are multiplexed onto the keystroke stream as
`InputKey.Mouse(event)`, so you handle them in the same `Sub.InputKey`
handler as everything else.

```scala
enum MouseEvent:
  case Press(button: MouseButton, col: Int, row: Int, modifiers: Modifiers)
  case Release(button: MouseButton, col: Int, row: Int, modifiers: Modifiers)
  case Drag(button: MouseButton, col: Int, row: Int, modifiers: Modifiers)
  case Move(col: Int, row: Int, modifiers: Modifiers)
  case Scroll(direction: ScrollDirection, col: Int, row: Int, modifiers: Modifiers)
```

`MouseButton` is `Left | Middle | Right | Other(code)`.
`ScrollDirection` is `Up | Down | Left | Right`.

For the standard click-and-drag flow, see the `screen` layer's
`HitTest` cache and `Layout.Zone` — together they map a coordinate to
a logical zone id without you re-deriving rectangles.

> File: `modules/termflow-terminal/src/main/scala/termflow/tui/Mouse.scala`.

## WCWidth — column-width arithmetic

Some characters take up two columns (CJK ideographs, most emoji),
some take zero (combining marks), some are control codes. Naïve
`String.length` lies about the visual width.

```scala
import termflow.tui.WCWidth

WCWidth.codePointWidth('A'.toInt)  // 1
WCWidth.codePointWidth('好'.toInt) // 2
WCWidth.charWidth('́')        // 0 (combining acute)
WCWidth.stringWidth("こんにちは")   // 10 (5 wide chars × 2)
```

The renderer uses these for layout, cursor placement, and diff
emission. Prompt cursor math (`Prompt.cursorColumn`) is one place this
shows up — typing `好` advances the cursor by two columns, not one.

> File: `modules/termflow-terminal/src/main/scala/termflow/tui/WCWidth.scala`.

## Grapheme — UAX #29 cluster boundaries

Backspace shouldn't delete half of a character. `é` written as `e +
U+0301` is two code points but one grapheme — pressing Backspace once
should remove both. `Grapheme` wraps `java.text.BreakIterator` to give
you the right boundaries:

```scala
import termflow.tui.Grapheme

Grapheme.previousBoundary("café", 4)  // 3   — back across "é"
Grapheme.nextBoundary("café", 0)      // 1
Grapheme.count("👋🏽")                // 1   (skin-tone modifier)
```

Used by `Prompt.handleKey` (Backspace, Delete, Arrow-left/right) and
`MultiLineInput`. If you're building your own text widget, route your
navigation through these.

> File: `modules/termflow-terminal/src/main/scala/termflow/tui/Grapheme.scala`.

## A working example: capability sniffer

A standalone CLI that prints what your terminal can do:

```scala
import termflow.tui.{Capabilities, JLineTerminalBackend}

@main def termcaps(): Unit =
  val backend = new JLineTerminalBackend()
  val caps    = backend.capabilities
  println(s"size:           ${backend.width} × ${backend.height}")
  println(s"colour depth:   ${caps.colorDepth}")
  println(s"unicode:        ${caps.unicode}")
  println(s"mouse:          ${caps.mouse}")
  println(s"extended styles:${caps.extendedStyles}")
  println(s"bracketed paste:${caps.bracketedPaste}")
  backend.close()
```

This is the kind of utility that doesn't need the runtime, doesn't
need the alt buffer, just wants to look at the TTY for a moment. The
terminal layer alone is enough.

## Reaching higher

When you need a draw surface and diff rendering, the
[Screen layer guide](screen-layer.md) is next. When you need an event
loop, model + update + view, focus management, dialogs — that's the
[Application layer guide](app-layer.md).

The full per-type API is in the [Scaladoc](../reference/api.md).
