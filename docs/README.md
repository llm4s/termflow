# TermFlow

`termflow` is a small, functional terminal UI (TUI) framework for Scala 3.

It is designed for building interactive command-line apps with a simple
architecture borrowed from Elm:

- a pure `update` function (state transitions),
- a `view` function returning a small virtual DOM,
- `Cmd` for async work,
- `Sub` for event streams (keys, timers, terminal resize).

The project started as the TUI layer for the
[llm4s](https://github.com/llm4s/llm4s) sample applications, and is now
usable on its own.

## Where to go next

- New here? Start with **[What is TermFlow?](intro/what-is-termflow.md)**
  for the elevator pitch and an architecture diagram, then jump into the
  **[Hello, World tutorial](tut/01-hello-world.md)**.
- Coming from another TUI library? The **[Architecture](intro/architecture.md)**
  page maps TermFlow's layers onto Lanterna and bubbletea equivalents.
- Looking for a specific widget? The **[Widgets guide](guide/widgets.md)**
  catalogues every shipped component.
- Need to test a TUI app? See the **[Testing guide](guide/testing.md)**.

## Project status

TermFlow is pre-1.0. Stage 3 (component breadth) is complete; Stage 4
(stabilisation) is in progress, and this site is part of it. APIs are still
allowed to change between minor versions until 1.0.0; see the
**[Migration notes](reference/migration.md)** when upgrading.
