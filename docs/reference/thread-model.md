# Thread model

TermFlow's runtime uses several threads behind a deliberately small set
of synchronisation points. This page documents the topology and the
invariants apps and widget authors can rely on, so you don't have to
reverse-engineer them from `TuiRuntime.scala`.

## Topology

```
                        ┌─────────────────────────┐
                        │   runtime thread        │
                        │   (the main loop)       │
                        │                         │
                        │   - drain CmdBus        │
                        │   - run TuiApp.update   │
                        │   - run TuiApp.view     │
                        │   - render frame        │
                        └────┬────────────┬───────┘
                             ▲            │
                             │ publish    │ writes ANSI
                             │            ▼
                        ┌────┴───┐    ┌───────────────┐
                        │ CmdBus │    │ TerminalBackend│
                        │ (queue)│    │ (JLine writer) │
                        └────▲───┘    └───────────────┘
                             │
              ┌──────────────┼──────────────┬──────────────────┐
              │              │              │                  │
   ┌──────────┴──┐   ┌───────┴────┐  ┌──────┴──────┐  ┌────────┴────────┐
   │ InputKey    │   │ Sub.Every  │  │ FCmd Future │  │ TerminalResize  │
   │ producer    │   │ scheduler  │  │ executor    │  │ SIGWINCH        │
   │ (one        │   │ (one       │  │ (global EC) │  │ listener        │
   │  thread     │   │  scheduled │  │             │  │ (JLine          │
   │  per Sub)   │   │  thread    │  │             │  │  callback       │
   │             │   │  per Sub)  │  │             │  │  thread)        │
   └─────────────┘   └────────────┘  └─────────────┘  └─────────────────┘
```

Every off-main-thread component talks to the runtime by publishing to
`CmdBus`. The runtime is the only consumer.

## Threads in detail

| Thread | Lifetime | Role |
|---|---|---|
| **runtime thread** | `TuiRuntime.run(app)` start → `Cmd.Exit` (or shutdown hook) | Executes `update`, `view`, and renders. Single-threaded. |
| **InputKey producer** | Lazy: started by the first `RuntimeCtx.registerSub` of a `Sub.InputKey` | Reads keystrokes off the `TerminalBackend.reader` and publishes `Cmd.GCmd` per parsed key. |
| **Sub.Every scheduler** | Lazy: per-`Sub.Every`, started on `registerSub` | A `ScheduledExecutorService` ticks at the configured period and publishes a `Cmd.GCmd` per tick. |
| **FCmd Future executor** | Per `Cmd.FCmd` — uses `ExecutionContext.global` by default | Runs the user's `Future` body off the runtime thread; the continuation publishes a result `Cmd` back. |
| **Resize listener** | Lazy on `Sub.TerminalResize` | JLine's SIGWINCH callback runs on its own thread; the listener publishes a `Cmd.GCmd` per resize. |
| **JVM shutdown hook** | Registered once by `TuiRuntime.run` | Restores cursor / leaves alt-buffer / disables mouse on abrupt exit. |

## Invariants you can rely on

1. **`update` and `view` always run on the runtime thread.** No two
   `update` calls overlap — the bus is a single consumer. Mutating
   `Model` inside `update` is therefore safe by construction (and the
   model should be immutable anyway).
2. **`Cmd.FCmd` continuations come back through the bus.** When your
   `Future[A]` completes, the result mapper runs *off* the runtime
   thread, but the produced `Cmd` is published — so the `update` that
   sees it runs on the runtime thread again. You don't need to
   synchronise on shared state, only on whatever the `Future`'s body
   itself touches.
3. **`Sub` callbacks run off their respective threads.** A `Sub.Every`
   tick fires on the scheduler thread, an `InputKey` parse fires on
   the producer thread. Whatever you do in the callback that produces a
   `Cmd` runs off-main, but the resulting `Cmd` always arrives at
   `update` on the runtime thread.
4. **Subscriptions start lazily.** `Sub.InputKey`, `Sub.Every`, and
   `Sub.TerminalResize` don't spawn threads or schedule timers until
   `RuntimeCtx.registerSub` is called. Tests using `TestRuntimeCtx`
   keep them dormant deliberately.
5. **`CmdBus` is a serialising queue.** Multiple producer threads can
   `publish` concurrently; the runtime's `take` / `poll` is the single
   consumer. Order across producers is FIFO by enqueue time.

## Gotchas

- **Don't block the runtime thread.** Anything synchronous inside
  `update` blocks the entire frame. Use `Cmd.FCmd` (or `Cmd.asyncResult`)
  for I/O.
- **Don't close over mutable state inside `FCmd`.** The body runs
  off-main, possibly concurrently with another `FCmd` body, and the
  continuation also runs off-main before the resulting `Cmd` is
  published. Pass the data you need by value into the `Future`.
- **`Sub.Every` tick drift is not corrected.** The scheduler uses
  `scheduleAtFixedRate`, which can drift if the runtime is slow to
  drain ticks. If exact wall-clock cadence matters, use
  `Sub.Every` for *triggering* and read `System.currentTimeMillis()`
  inside `update` for the *timestamp*.
- **JLine callbacks (resize, signals) are not on a TermFlow-managed
  thread.** Don't do anything in those callbacks except publish to
  `CmdBus`.

## Subscription cleanup

`CmdBus.cancelAllSubscriptions()` is called by the runtime on exit
(both clean exit via `Cmd.Exit` and abrupt exit via the shutdown hook).
Each `Sub`'s `cancel()` is best-effort: it may interrupt the producer
thread, shut down a scheduler, or close a `Reader`. If one cancel
throws, the rest still run.

## Testing

For deterministic tests, use the testkit:

- `TestRuntimeCtx` keeps subs dormant — `registerSub` does not call
  `start()`.
- `TuiTestDriver` advances the model by feeding `Msg`s directly,
  bypassing the bus and threading concerns.
- `KeySim` and `MouseSim` synthesise input events.

Real-time behaviour (`Sub.Every` cadence, JLine reader latency) is
deliberately *not* covered by the testkit — that's what the sample apps
exist for.
