package termflow

/**
 * # TermFlow
 *
 * An Elm-style TUI framework for Scala 3.
 *
 * Apps describe their state as a `Model`, react to inputs via a pure
 * `update`, render a virtual DOM via `view`, and let
 * [[termflow.tui.TuiRuntime]] handle the loop, terminal I/O, and frame
 * coalescing.
 *
 * ## Five core types
 *
 *   - [[termflow.tui.TuiApp]] — the application contract: `init` /
 *     `update` / `view` / `toMsg`. Most apps are an `object` extending
 *     this trait.
 *   - [[termflow.tui.Tui]] — a `(Model, Cmd[Msg])` pair returned by
 *     `init` and `update`.
 *   - [[termflow.tui.Cmd]] — effects: `NoCmd`, `Exit`, `GCmd(msg)`,
 *     `FCmd(future)`, `TermFlowErrorCmd(err)`.
 *   - [[termflow.tui.Sub]] — subscriptions for keyboard input, timers,
 *     and resize polling. Use `Sub.InputKey`, `Sub.Every`,
 *     `Sub.TerminalResize`.
 *   - [[termflow.tui.VNode]] — the virtual DOM: `TextNode`, `BoxNode`,
 *     `InputNode`, wrapped in a [[termflow.tui.RootNode]] returned by
 *     `view`.
 *
 * ## Composition stack
 *
 *   - [[termflow.tui.Layout]] composes children into a flat list of
 *     positioned `VNode`s without hand-rolled coordinates.
 *   - [[termflow.tui.widgets.Button]] /
 *     [[termflow.tui.widgets.ProgressBar]] /
 *     [[termflow.tui.widgets.Spinner]] /
 *     [[termflow.tui.widgets.StatusBar]] in `termflow.tui.widgets`
 *     provide ready-to-render building blocks; each takes a `given`
 *     [[termflow.tui.Theme]].
 *   - [[termflow.tui.Theme]] supplies semantic colour slots (`primary`,
 *     `success`, `error`, …) so apps don't bake in palettes. Built-ins:
 *     `Theme.dark`, `Theme.light`, `Theme.mono`.
 *   - [[termflow.tui.FocusManager]] tracks ordered focusable elements
 *     for forms and button rows; [[termflow.tui.Keymap]] turns key
 *     events into messages declaratively.
 *
 * ## A minimal runnable example
 *
 * {{{
 * import termflow.tui.*
 * import termflow.tui.TuiPrelude.*
 *
 * object Counter:
 *   final case class Model(count: Int)
 *
 *   enum Msg:
 *     case Inc, Dec, Quit
 *
 *   object App extends TuiApp[Model, Msg]:
 *     def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] = Model(0).tui
 *
 *     def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
 *       msg match
 *         case Msg.Inc  => m.copy(count = m.count + 1).tui
 *         case Msg.Dec  => m.copy(count = m.count - 1).tui
 *         case Msg.Quit => Tui(m, Cmd.Exit)
 *
 *     def view(m: Model): RootNode =
 *       RootNode(
 *         width    = 40,
 *         height   = 1,
 *         children = List(TextNode(1.x, 1.y, List(s"count = \${m.count}".text))),
 *         input    = None
 *       )
 *
 *     def toMsg(input: PromptLine): Result[Msg] = Right(Msg.Inc)
 *
 *   @main def run(): Unit = TuiRuntime.run(App)
 * }}}
 *
 * ## Testing apps
 *
 * The `termflow.testkit` package (test classpath, available via
 * `dependsOn(termflow % "test->test")`) provides `TuiTestDriver` — a
 * synchronous driver that runs apps without `TuiRuntime` and captures
 * frames as cell matrices for golden-file comparison via `GoldenSupport`.
 *
 * ## Further reading
 *
 *   - `docs/DESIGN.md` — architecture and motivation
 *   - `docs/RENDER_PIPELINE.md` — coalescing, diff rendering, and
 *     snapshot testing conventions
 *   - `docs/RUN_EXAMPLES.md` — runnable sample apps and how to launch
 *     them
 */
package object tui
