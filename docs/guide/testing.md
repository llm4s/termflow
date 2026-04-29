# Testing

> *Stub — full content lands in Phase C of the docs roll-out.*

This guide will cover `termflow-testkit`: deterministic harness for
TermFlow apps without a real terminal.

Topics planned:

- `TuiTestDriver` — synchronous `init` / `send` / `frame` /
  `model` / `exited` driver.
- Asserting on the rendered frame (cell-grid comparison vs. golden
  snapshots via `GoldenSupport`).
- `KeySim` and `MouseSim` — typed key and mouse event constructors,
  including `Tab`, `Enter`, `CharKey`, and SGR-1006 mouse events that
  flow through `InputKey.Mouse`.
- `TestRuntimeCtx` — keeps subscriptions dormant so timer ticks don't
  fire during tests.

Until the page is filled in, the existing test suites under
[`modules/termflow-sample/src/test/scala`](https://github.com/llm4s/termflow/tree/main/modules/termflow-sample/src/test/scala)
are good worked examples — the wizard, showcase, and form-demo specs
all use `TuiTestDriver`, `KeySim`, and `MouseSim`.
