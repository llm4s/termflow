# Widgets

> *Stub — full content lands in Phase C of the docs roll-out.*

This guide will catalogue every widget in `termflow-widgets` with one
paragraph per widget, a screenshot, and a code snippet showing the
common usage. Until then, the
[showcase app](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/showcase/Stage1ShowcaseApp.scala)
exercises every widget and the
[Scaladoc](../reference/api.md) has the full per-widget API.

The widgets, grouped:

**Inputs**

- `TextField` — single-line text input
- `MultiLineInput` — multi-line editor with grapheme-aware navigation
- `Button`
- `CheckBox` / `RadioGroup`
- `Select` / `Autocomplete`
- `Prompt` (in `termflow-app`, not `widgets`)

**Data**

- `ListView`
- `Table`
- `Tree`

**Layout**

- `Tabs`
- `SplitPane` (with mouse-drag resize)
- `Separator` (horizontal / vertical)
- `ScrollBar`

**Feedback**

- `ProgressBar`
- `Spinner`
- `StatusBar`
- `LogView`
