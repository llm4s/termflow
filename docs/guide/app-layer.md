# Application layer

> *Stub — full content lands in Phase C of the docs roll-out.*

This guide covers `termflow-app` — the runtime loop, the `TuiApp`
contract, `Cmd`, `Sub`, `FocusManager`, and the `Dialogs` helpers.

Topics planned for this page:

- The `TuiRuntime` driver and the `CmdBus` blocking queue.
- The four-method `TuiApp[Model, Msg]` contract in detail.
- The `Cmd` ADT — `NoCmd`, `Exit`, `GCmd`, `FCmd`, `TermFlowErrorCmd`.
- Subscriptions — `Sub.InputKey`, `Sub.Every`, `Sub.TerminalResize`,
  and the `RuntimeCtx` auto-registration pattern.
- `FocusManager` — focus order, cycling, and explicit `focus(id)`.
- `Dialogs` overlays — confirm, textInput, listSelect, waiting,
  fileDialog, directoryDialog, actionList.

This layer is what most TermFlow users build on top of. The
[Hello, World tutorial](../tut/01-hello-world.md) is the gentlest
introduction; the [Counter tutorial](../tut/02-counter.md) covers
synchronous `update` patterns; the
[Async work tutorial](../tut/03-async.md) covers `Cmd.FCmd` and
`Sub.Every`.
