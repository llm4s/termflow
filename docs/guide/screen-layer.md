# Screen layer

> *Stub — full content lands in Phase C of the docs roll-out.*

This guide covers `termflow-screen` — the buffered character grid,
layout DSL, hit-test cache, and ANSI diff renderer.

Topics planned for this page:

- `RenderFrame` and `RenderCell` — the cell grid and its style fields.
- `AnsiRenderer` — diff-painting, cursor placement, capability gates.
- `Layout` — the `Row` / `Column` / `Elem` / `Spacer` / `Fill` / `Zone`
  DSL and how `resolve` produces absolute coordinates.
- `HitTest[Id]` and `Layout.resolveTracked[Id]` — the layout-pass
  hit-test cache used by the showcase to map mouse clicks to logical
  zones.
- When to use `termflow-screen` directly without the app layer.

Until the page is filled in, the
[render pipeline doc](../contrib/RENDER_PIPELINE.md) is the most thorough
write-up.
