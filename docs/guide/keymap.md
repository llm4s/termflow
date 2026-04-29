# Keymap

`Keymap` turns ad-hoc `match` blocks against `KeyDecoder.InputKey`
into declarative bindings. The win is a single
`Map[InputKey, Msg]` you can pass around, merge, and override —
useful for global shortcuts, modal overrides, and feature flags.

```scala
import termflow.tui.Keymap
import termflow.tui.KeyDecoder.InputKey
```

## Building a keymap

```scala
val km: Keymap[Msg] = Keymap(
  InputKey.CharKey('q')   -> Msg.Quit,
  InputKey.CharKey('?')   -> Msg.ToggleHelp,
  InputKey.Tab            -> Msg.NextFocus,
  InputKey.BackTab        -> Msg.PrevFocus
)

km.lookup(InputKey.Tab)   // Some(NextFocus)
km.lookup(InputKey.F1)    // None
```

`Keymap[Msg]` is just `Map[InputKey, Msg]` under the hood — equality,
size, etc. work as you'd expect.

## Composing keymaps

Two keymaps merge with `++`. The right-hand side wins on conflict —
so put global bindings first, mode-specific bindings last:

```scala
val global    = Keymap.quit(Msg.Quit) + (InputKey.CharKey('?') -> Msg.ToggleHelp)
val editing   = Keymap(InputKey.Escape -> Msg.LeaveEditMode)

// In edit mode: editing's Escape wins over a global Escape, if any.
val active    = global ++ editing
```

The `+` operator adds or replaces a single binding:

```scala
val withSave  = global + (InputKey.Ctrl('S') -> Msg.Save)
```

## Builders for common patterns

```scala
Keymap.empty[Msg]                                  // {}
Keymap(bindings: (InputKey, Msg)*)                 // arbitrary
Keymap.quit(Msg.Quit)                              // Ctrl+C, Esc, q, Q
Keymap.focus(next = Msg.NextFocus, previous = Msg.PrevFocus)
                                                   // Tab + Shift+Tab
Keymap.focusVertical(previous = Msg.Up, next = Msg.Down)
                                                   // ArrowUp + ArrowDown
Keymap.focusHorizontal(previous = Msg.Left, next = Msg.Right)
                                                   // ArrowLeft + ArrowRight
```

These are the four most common shapes — quitting, tab focus, vertical
focus, horizontal focus. Stack them with `++` to assemble an app's
global keymap.

## Wiring into `update`

```scala
case Msg.KeyPressed(key) =>
  km.lookup(key) match
    case Some(mapped) => Tui(m, Cmd.GCmd(mapped))
    case None         =>
      // Unmapped key — fall through to widget-specific handling.
      m.tui
```

The `Cmd.GCmd(mapped)` re-enters `update` with the mapped message,
which `update` handles like any other domain event. This decouples
"which keys do what" from "what happens when something is done".

For widgets that swallow keystrokes (TextField, MultiLineInput,
Prompt), feed the key into the widget *first*, and only fall back to
`Keymap.lookup` when the widget doesn't consume it. The
[forms tutorial](../tut/04-forms-and-dialogs.md#6-focus-dispatch)
walks through this pattern.

## Mode stacks

Apps with insert / normal / command modes can stack keymaps:

```scala
final case class Model(
  mode: EditorMode,                 // Normal | Insert | Command
  /* … */
):
  def keymap: Keymap[Msg] = mode match
    case Normal  => globalKm ++ normalKm
    case Insert  => globalKm ++ insertKm
    case Command => globalKm ++ commandKm
```

`update` always calls `model.keymap.lookup(key)`. Mode transitions
just change which keymap is active.

## Help overlays

Because `Keymap` is just a map, you can render a help screen straight
from the bindings:

```scala
def renderHelp(km: Keymap[Msg], labelOf: Msg => String): List[VNode] =
  km.bindings.toList.sortBy(_._2.toString).map { case (key, msg) =>
    TextNode(2.x, 1.y, List(
      describe(key).text(fg = Theme.dark.primary),
      "  ".text,
      labelOf(msg).text
    ))
  }
```

`describe(InputKey)` is whatever pretty-printer you want — the
`Keymap.scala` source has a `renderChord` helper that returns
human-readable strings (`"Tab"`, `"Ctrl+S"`, `"Esc"`).

## Reference

> File: `modules/termflow-app/src/main/scala/termflow/tui/Keymap.scala`.

For a working example, see
[`apps.forms.FormDemoApp`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/forms/FormDemoApp.scala)
— it builds a global keymap, layers a per-field keymap on top, and
renders the active bindings as a help footer.
