# Theming

`Theme` is a small struct of named colour slots plus a `BorderChars`
glyph set. Widgets and dialogs that take `(using Theme)` pick up the
ambient theme without needing to know what your palette looks like.

```scala
import termflow.tui.Theme
```

## The shape

```scala
final case class Theme(
  primary:    Color,  secondary:  Color,
  error:      Color,  warning:    Color,
  success:    Color,  info:       Color,
  border:     Color,  background: Color,  foreground: Color,
  chars:      BorderChars = BorderChars.sharp
)

object Theme:
  val dark:    Theme   // shipped — default
  val light:   Theme   // shipped — light palette
  val mono:    Theme   // shipped — single-colour terminal fallback
  val rounded: Theme   // shipped — rounded box-drawing chars
```

Pick one as your `given` at the top of your view code:

```scala
import termflow.tui.{Theme, Color}
import termflow.tui.Theme.themed

given Theme = Theme.dark

val tag = "PRIMARY".themed(_.primary)
val ok  = "✓".themed(_.success)
```

The `.themed(slot)` extension takes a `Theme => Color` and returns a
styled `Text`. This keeps view code free of hard-coded colours and
gives you palette-swap-by-import.

## Slot semantics

The names are deliberately abstract — they mean roles, not literal
colours:

| Slot        | Used for |
|-------------|----------|
| `primary`   | Headings, focus chrome, active tab |
| `secondary` | Subtle headings, placeholder text |
| `error`     | Validation messages, exit codes |
| `warning`   | Caution states |
| `success`   | Confirmation messages, "✓ Submitted" |
| `info`      | Neutral status (timestamps, counts) |
| `border`    | Box outlines, separators |
| `background`| Fills (rare — the terminal default usually wins) |
| `foreground`| Default text colour |

The four shipped themes pick concrete `Color` values for each slot
that look good together. When you add your own theme, copy
`Theme.dark` and override what you need:

```scala
val termflowBranded: Theme = Theme.dark.copy(
  primary  = Color.Rgb(0xff, 0x6f, 0x3d),  // brand orange
  border   = Color.Indexed(238)
)
```

## Capability gating

`Color.Rgb(...)` only emits true-colour ANSI when the terminal
supports it. The `AnsiRenderer` reads `Capabilities.colorDepth` (from
the `Capabilities.detect(env)` pass at startup) and downgrades:

```text
Truecolor → Indexed256 → Ansi16 → Ansi8 → Mono
```

So an `Rgb(0xff, 0x6f, 0x3d)` becomes the closest 256-colour
approximation on terminals that don't speak true-colour, and a basic
8-colour ANSI on really old terminals.

`NO_COLOR` is honoured — set the env var and the renderer drops to
`Mono`. For everything `colour-blind`-related, prefer the `mono`
theme rather than rolling your own.

## Box-drawing chars

```scala
object BorderChars:
  val sharp:   BorderChars  // ┌─┐│└┘
  val rounded: BorderChars  // ╭─╮│╰╯
  val double:  BorderChars  // ╔═╗║╚╝
  val ascii:   BorderChars  // +-+|-+
```

When `Capabilities.unicode` is `false`, the renderer auto-substitutes
`ascii` regardless of which set the theme picked. This means you can
ship `Theme.dark` (which uses `sharp`) and it still renders correctly
on a vt100.

To switch glyphs in code:

```scala
val rounded = Theme.dark.copy(chars = BorderChars.rounded)
```

## A theme-aware widget

Most widgets in the catalogue already take `(using Theme)` — that's
all you have to know to use them. If you're building your own
widget, the same pattern applies:

```scala
def MyBadge(label: String)(using theme: Theme): VNode =
  TextNode(1.x, 1.y, List(
    " ".text,
    label.text(fg = theme.background, bg = theme.primary, bold = true),
    " ".text
  ))
```

## Switching at runtime

Because `Theme` is just a value, you can swap themes from `update`
and pass it through to `view` via the model:

```scala
final case class Model(theme: Theme, /* … */)

def view(m: Model): RootNode =
  given Theme = m.theme
  // … render against the active theme
```

The
[`apps.themes.ThemeDemoApp`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/themes/ThemeDemoApp.scala)
sample shows side-by-side previews of every shipped theme — useful
both as a smoke test for new themes and as a starting point if you're
designing your own palette.

## Reference

> File: `modules/termflow-screen/src/main/scala/termflow/tui/Theme.scala`.

For the full per-slot API, see the [Scaladoc](../reference/api.md).
