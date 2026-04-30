# Cookbook

Short "how do I…" recipes. Each one is one or two screens of
explanation plus a self-contained snippet, grounded in actual
TermFlow APIs and (where possible) a sample app you can run.

## Recipes

- **[Show a confirm dialog and act on the answer](confirm-dialog.md)**
  — `Dialogs.confirm`, model-owned dialog state, key routing while
  modal.
- **[Stream output into a scrollback view](streaming-output.md)** —
  `widgets.LogView`, append-and-tail buffering, auto-tail vs. paused
  scroll.
- **[Pause and resume a `Sub.Every` timer](timer-pause-resume.md)** —
  cancel + recreate, `Sub.NoSub` placeholder, interval changes.
- **[Open a file picker and load the result](file-picker.md)** —
  `Dialogs.fileDialog` integration pattern, directory navigation,
  Esc-to-cancel.
- **[Two-pane layout with a draggable splitter](split-pane-drag.md)**
  — `SplitPane.handleMouse`, `DragState`, keyboard-equivalent
  shortcuts.
- **[Capture mouse clicks on a custom widget](mouse-on-custom-widget.md)**
  — `Layout.Zone` + `Layout.resolveTracked` + `HitTest[Id]`.
- **[Wide-character (CJK / emoji) input handling](wide-character-input.md)**
  — `WCWidth`, `Grapheme`, `RenderCell.width = 2`.
- **[Clean shutdown on `Ctrl-C` and on resize](clean-shutdown.md)** —
  what the runtime does for you, where you still have to wire `Cmd.Exit`.
- **[Flag a session as needing attention](notifications.md)** —
  `Cmd.RequestAttention`, `Cmd.Notify`, terminal detection
  (iTerm2 / kitty / VTE), tmux caveats.
- **[Full-screen layouts that reflow on resize](full-screen-layout.md)** —
  `Layout.toBudgetedRootNode` vs. `toRootNode`, `Fill` semantics,
  header/fill/footer pattern.

## Want more?

Recipe gaps are tracked as GitHub issues. If you've got a "how do I
do X" that isn't covered, please
[open an issue](https://github.com/llm4s/termflow/issues) — most
recipes start as a question that came up twice.
