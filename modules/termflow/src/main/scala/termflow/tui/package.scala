/**
 * TermFlow is an Elm-architecture TUI framework for Scala 3. An application
 * defines a model, a message type, and four functions (`init`, `update`,
 * `view`, `toMsg`); the runtime calls them in a loop and paints the rendered
 * virtual DOM to the terminal.
 *
 * == Core types ==
 *
 * Every TermFlow app works with the same small vocabulary:
 *
 *  - [[termflow.tui.TuiApp]] — the trait your application extends. Wires
 *    together `init` / `update` / `view` / `toMsg` and gives the runtime
 *    enough structure to drive your program.
 *  - [[termflow.tui.Tui]] — the `(Model, Cmd[Msg])` pair returned from
 *    `init` and `update`. `Model.tui` lifts a model into a `Tui` with
 *    `Cmd.NoCmd`; `Model.gCmd(msg)` lifts with a `GCmd(msg)`.
 *  - [[termflow.tui.Cmd]] — imperative effects the runtime executes on your
 *    behalf: send another message (`GCmd`), run a `Future` (`FCmd`), exit
 *    (`Exit`), or report an error (`TermFlowErrorCmd`).
 *  - [[termflow.tui.Sub]] — long-running sources of messages: keyboard
 *    input, timers, terminal resize. Register with `RuntimeCtx.registerSub`
 *    so the runtime cancels them on exit.
 *  - [[termflow.tui.VNode]] — the virtual DOM that `view` produces. Text,
 *    boxes, and inputs are positioned with `XCoord` / `YCoord` and styled
 *    with [[termflow.tui.Style]] / [[termflow.tui.Color]].
 *
 * == Message loop ==
 *
 * The runtime starts by calling `init(ctx)` to build the first `Tui[Model,
 * Msg]`. `toMsg` turns each user-entered prompt line into a `Msg`;
 * subscription events (keyboard, timers, resize) are published as
 * `Cmd.GCmd(msg)` and dispatched straight to `update` without going through
 * `toMsg`. Either path lands at `update(model, msg, ctx)`, which produces
 * the next `Tui`. `view(model)` renders after every update and the
 * resulting `RootNode` is diffed and written to the terminal by
 * `AnsiRenderer`.
 *
 * == Pointers ==
 *
 *  - Architecture overview: `docs/DESIGN.md`
 *  - Rendering internals: `docs/RENDER_PIPELINE.md`
 *  - Running the sample apps: `docs/RUN_EXAMPLES.md`
 *
 * == Sample apps ==
 *
 * The `termflow-sample` module contains runnable apps that exercise the
 * framework end-to-end. Small starting points:
 *
 *  - `termflow.apps.task.RenderApp` — a minimal `TuiApp` wiring up
 *    `init` / `update` / `view` / `toMsg`.
 *  - `termflow.apps.echo.EchoApp` — prompt-line loop with a scrolling
 *    history.
 *  - `termflow.apps.tabs.TabsDemoApp` — a larger demo that composes
 *    several `VNode` shapes.
 *
 * See `docs/RUN_EXAMPLES.md` for how to run them.
 */
package termflow.tui
