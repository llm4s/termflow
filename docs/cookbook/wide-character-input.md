# Wide-character (CJK / emoji) input handling

CJK ideographs and most emoji take **two terminal cells**. Combining
marks like the acute accent in `é = e + ́` take **zero**. Naïve
`String.length` is wrong about both. Render and edit code that
ignores this clips characters, miscounts cursor positions, and
leaves stray cells when text shrinks.

TermFlow's prompt and multi-line widgets handle this for you out of
the box. If you're building your own text widget, the rules are
straightforward.

## Pattern

Three primitives:

| Need | Use |
|---|---|
| How wide is this char/string? | `WCWidth.charWidth(c)` / `stringWidth(s)` |
| Where's the previous grapheme boundary? | `Grapheme.previousBoundary(s, idx)` |
| Where's the next? | `Grapheme.nextBoundary(s, idx)` |

Apply them in two places:

1. **Cursor column math.** When rendering a text input, the cursor's
   *visual* column is `WCWidth.stringWidth(text.take(charIndex))`,
   not `charIndex` itself.
2. **Backspace / Delete.** Move by graphemes, not by chars or code
   points.

## Code: cursor column

```scala
import termflow.tui.WCWidth

def cursorColumn(buffer: String, charIndex: Int): Int =
  WCWidth.stringWidth(buffer.take(charIndex))

cursorColumn("hello", 3)        // 3
cursorColumn("こんにちは", 3)   // 6  (3 wide chars × 2)
cursorColumn("👋🏽 hi", 3)      // 4  (👋🏽 = 2 cells, space = 1)
```

`Prompt.cursorColumn(state)` does exactly this — see
`modules/termflow-app/src/main/scala/termflow/tui/Prompt.scala`.

## Code: grapheme-aware Backspace

```scala
import termflow.tui.Grapheme

def backspace(buffer: String, cursor: Int): (String, Int) =
  if cursor == 0 then (buffer, 0)
  else
    val prev    = Grapheme.previousBoundary(buffer, cursor)
    val without = buffer.substring(0, prev) + buffer.substring(cursor)
    (without, prev)

backspace("café", 4)   // ("caf", 3) — drops "é" cleanly even though it's 2 chars
backspace("👋🏽hi", 4)  // ("hi", 0)  — drops the whole emoji+modifier
```

`Prompt.handleKey` already does this internally for `Backspace`,
`Delete`, `ArrowLeft`, `ArrowRight`. If you reach for it, you don't
have to.

## Code: rendering wide cells in a custom widget

```scala
import termflow.tui.WCWidth

// When laying out one row of text into cells, advance by width(c):
def stringToCells(text: String, style: Style): List[RenderCell] =
  val out = List.newBuilder[RenderCell]
  text.foreach { ch =>
    WCWidth.charWidth(ch) match
      case 1 => out += RenderCell(ch, style, width = 1)
      case 2 => out += RenderCell(ch, style, width = 2)  // takes two cells
      case 0 => /* combining; skip — should fold into prior cell */
      case _ => /* control; skip */
  }
  out.result()
```

The `RenderCell.width = 2` flag is what tells `AnsiRenderer` to
*skip* the next cell — it knows the wide glyph already covers it.

## Notes

- **Don't mix `String.length` with cell math.** They aren't the
  same. If you find yourself comparing one to the other, you almost
  certainly have a bug.
- **`Grapheme.count(s)`** returns the number of user-visible
  characters, not cells. It's the right thing for "how many
  characters has the user typed?", which is rarely what you actually
  want for layout.
- **`NO_COLOR` and `LANG=C`.** Some terminals are configured to
  refuse Unicode. The `Capabilities.unicode` flag tells you whether
  to use ASCII fallbacks (`Theme` already does this for box-drawing
  glyphs).
- **Combining sequences.** `Grapheme.previousBoundary` walks across
  full graphemes including ZWJ sequences (👨‍👩‍👧‍👦), skin-tone
  modifiers (👋🏽), and regional indicators (🇯🇵🇺🇸).

For a working example, see the showcase's *Inputs* tab — type CJK
and emoji into the `MultiLineInput` and watch the cursor track
correctly.
