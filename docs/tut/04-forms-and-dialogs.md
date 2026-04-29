# Forms and dialogs

> *Stub — full content lands in Phase B of the docs roll-out.*

This tutorial will cover `FocusManager`, the `Form` builder, and the
shipped `Dialogs` helpers (confirm, textInput, listSelect, waiting,
fileDialog, actionList), grounding everything in
[`termflow.apps.wizard.WizardApp`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/wizard/WizardApp.scala).

Headline points:

- Per-step `FocusManager` and `Tab`/`Shift+Tab` cycling.
- Routing keystrokes by current focus (the *focus dispatch* pattern).
- `Form.column` validation and the `errors` map.
- Mounting a dialog as an overlay with a continuation.
- The `Submit` button and exit flow.

For now, run it with `sbt wizardDemo`.
