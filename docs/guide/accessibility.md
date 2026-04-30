# Accessibility

TermFlow targets terminal apps, where the assistive-technology story is
genuinely different from the web: screen readers read the terminal
buffer directly, so the practical levers a TUI library has are around
**colour**, **motion**, and **predictable structure**.

This page documents the levers TermFlow exposes today.

## Colour

- `Theme.dark`, `Theme.light`, and `Theme.mono` ship as built-in palettes.
- Capability detection downgrades cleanly: true-colour → 256 → 16 → 8 →
  mono. Apps don't need to branch on the terminal — pick a `Theme` and
  let the renderer downgrade.
- The `NO_COLOR` environment variable is honoured: when set, the runtime
  forces `Theme.mono`-equivalent rendering regardless of detected
  capability.

## Reduced motion

Some users find ambient animation actively unhelpful — vestibular
sensitivity, low-bandwidth sessions, screen-reader environments that
re-announce on every frame, or just personal preference. TermFlow
exposes a single flag for this: `reducedMotion`.

### Activating reduced motion

Either set the environment variable:

```bash
TERMFLOW_REDUCED_MOTION=1
```

…or set it in the HOCON config:

```hocon
termflow {
  accessibility {
    reduced-motion = true
  }
}
```

The env var takes precedence over the config value when set. Truthy
values are anything other than `"0"` or `"false"` (case-insensitive).

### What it affects

The flag is plumbed through `RuntimeCtx.config.accessibility.reducedMotion`
so apps and widgets can read it.

`Spinner` accepts a `reducedMotion: Boolean` parameter that, when
`true`, pins the rendered frame to `frames(0)` regardless of the tick:

```scala
import termflow.tui.widgets.Spinner

def view(model: Model)(using ctx: RuntimeCtx[Msg]): VNode =
  Spinner(
    Spinner.Braille,
    frame = model.tick,
    reducedMotion = ctx.config.accessibility.reducedMotion
  )
```

App-level animation (sine wave demos, custom progress effects, anything
driven by `Sub.Every` purely for cosmetic reasons) should query the
flag and either skip rendering or fall back to a static representation.

### What it does not affect

- **Functional motion** — cursor movement, focus changes, scroll
  position, dialog open/close. Those are part of the app's behaviour,
  not cosmetic animation.
- **Determinate `ProgressBar`** — already non-cycling. Only
  indeterminate progress indicators count as cosmetic motion, and
  TermFlow's `ProgressBar` is determinate-only.

## Predictable structure

The Elm-style architecture (`update` is pure, `view` is a function of
the model) means the rendered frame is fully determined by the model.
That's a foundation accessibility tooling can build on — semantic
annotations, a linear/announcement renderer, or a screen-reader bridge.
Those are post-1.0 work; see the roadmap for current status.
