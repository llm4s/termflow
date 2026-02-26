# Render Pipeline

TermFlow now uses a coalesced runtime scheduler with framebuffer diff rendering to reduce flicker under high-frequency updates.

## Pipeline

```text
state events
  -> mark frame dirty
  -> coalesce queued commands
  -> build latest view (once per commit)
  -> materialize next frame buffer
  -> diff(previous, current)
  -> emit minimal ANSI patch
  -> restore logical input cursor
```

## Key Invariants

1. At most one render commit is active at a time.
2. Rendering is latest-state wins; intermediate states can be dropped during bursts.
3. Diff output includes cleanup for shrink cases (line tail and removed rows).
4. Cursor visibility is runtime-owned (startup/shutdown/interrupt), not frame-owned.
5. Each frame writes one buffered ANSI payload from renderer perspective.

## Why Flicker Dropped

- Most frames update a small subset of cells instead of repainting everything.
- Command bursts are coalesced before render, avoiding redundant intermediate paints.
- Cursor movement is deterministic and restored after body updates.

## Regression Focus Areas

- Prompt/input updates must not leave stale tail characters.
- Shrinking content must clear removed rows/columns.
- Borders at terminal edges must remain stable across rapid updates.
- Ctrl-C / unexpected exits must always restore cursor visibility.
