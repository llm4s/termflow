# Async work

The Counter tutorial built a synchronous counter — `Increment` ran inline
on the runtime thread. Real apps rarely have that luxury: HTTP calls, LLM
streams, file I/O, and database queries all need to happen *off* the
event loop while the UI keeps drawing.

This tutorial swaps the synchronous counter for an asynchronous one and
shows three patterns:

1. **`Cmd.FCmd`** — bridge a `Future` into the runtime so its result
   shows up as a `Msg`.
2. **`Sub.Every`** — fire a recurring `Msg` on a fixed cadence, which
   we'll use to animate a spinner while async work is in flight.
3. **Subscription lifecycle** — start a sub when work begins, cancel it
   when work ends.

Source: [`termflow.apps.counter.FutureCounter`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/counter/FutureCounter.scala).
Run with `sbt futureDemo`.

## What you'll build

```text
┌──────────────────────────────────────┐
│ Current count: 2                     │
│ incrementing::14:22:07 /             │
│                                      │
│ Commands:                            │
│   increment | + -> increase counter  │
│   decrement | - -> decrease counter  │
│   exit          -> quit              │
└──────────────────────────────────────┘
[]> _
```

The `/` rotates through `| / - \` while the async work runs (~5 seconds
for increment, ~10 for decrement). Once the work completes, the spinner
disappears and the count updates.

## 1. Two `Counter` shapes

The domain stays minimal — same `Counter` opaque type, with async
variants alongside the sync ones:

```scala
opaque type Counter = Int

object Counter:
  def apply(count: Int): Counter = count

  extension (c: Counter)
    def count: Int = c

    def asyncIncrement()(using ec: ExecutionContext): Future[Counter] =
      Future:
        Thread.sleep(5000)
        Counter(c.count + 1)

    def asyncDecrement()(using ec: ExecutionContext): Future[Counter] =
      Future:
        Thread.sleep(10000)
        Counter(c.count - 1)
```

`Future` is a stand-in for "any async work". In a real app it would be
an HTTP call or a database round-trip, not `Thread.sleep`.

## 2. A richer model

The model carries everything Counter had, plus enough state to drive a
spinner:

```scala
final case class Model(
  terminalWidth: Int,
  terminalHeight: Int,
  count: Counter,
  status: String,         // human-readable status line
  input: Sub[Msg],        // keyboard subscription
  prompt: Prompt.State,
  spinner: Sub[Msg],      // timer subscription — Sub.NoSub when idle
  spinnerIndex: Int       // current frame of the spinner animation
)
```

Two new fields worth highlighting:

- **`status: String`** is the line under the count. We'll write
  `"incrementing::14:22:07"` while work is in flight,
  `"done::14:22:12"` when it completes. Useful for both humans and for
  asserting in tests.
- **`spinner: Sub[Msg]`** is *the* live timer subscription. When idle
  it's `Sub.NoSub` — a no-op sub. When async work starts we replace it
  with `Sub.Every(200ms, …)`. When the work finishes we call
  `spinner.cancel()`.

## 3. Five new messages

```scala
enum Msg:
  case Increment
  case Decrement
  case Exit
  case UpdateWith(counter: Counter)
  case Busy(action: String)
  case SpinnerTick
  case ConsoleInputKey(key: KeyDecoder.InputKey)
  case ConsoleInputError(error: Throwable)
```

Compared to Counter:

- **`Increment` / `Decrement`** no longer carry the state change inline
  — they just *kick off* the async work.
- **`UpdateWith(c)`** is the message produced when the `Future`
  completes with a new counter value.
- **`Busy(action)`** is dispatched the moment work is enqueued so the
  UI can flip into "working" mode immediately, not after the future
  completes.
- **`SpinnerTick`** is the Msg the timer fires on every tick.

The split between `Increment` / `Busy` / `UpdateWith` is the
**bracketing pattern** that comes up over and over in async TUIs:

```text
Increment ─▶ FCmd kicks off Future + dispatches Busy
                                          │
                                          ▼
                                    Busy ─▶ start spinner
                                              │
                                              ▼
                                       SpinnerTick × N
                                              │
                                              ▼
                                  UpdateWith ─▶ stop spinner
```

## 4. `Cmd.FCmd` — bridging `Future` into the runtime

Inside `update`, the `Increment` arm returns this:

```scala
case Increment =>
  Tui(
    sized,
    Cmd.FCmd(
      sized.count.asyncIncrement(),
      (c: Counter) => Cmd.GCmd(UpdateWith(c)),
      onEnqueue = Some(Busy(s"incrementing::${TimeFormatter.getCurrentTime}"))
    )
  )
```

`Cmd.FCmd` takes three things:

| Parameter | What |
|---|---|
| `task` | The `Future[A]` you want the runtime to await. |
| `toCmd` | A function from the future's result to the next `Cmd`. |
| `onEnqueue` | An optional `Msg` to dispatch *immediately* when the FCmd is enqueued — before the future completes. |

What happens at runtime:

1. `update` returns the `Tui(model, FCmd(...))`.
2. The runtime sees the `FCmd`, registers a callback on the future, and
   if `onEnqueue` was supplied, immediately dispatches that `Msg`.
3. When the future completes (after ~5 seconds here),
   `toCmd(result)` runs and the resulting `Cmd` (here
   `Cmd.GCmd(UpdateWith(c))`) is enqueued.

The result: `Busy("incrementing::...")` arrives within microseconds of
pressing Enter, and `UpdateWith(newCount)` arrives ~5 seconds later.

> If your future already returns `Result[A]` (i.e. it can fail with a
> `TermFlowError`), reach for `Cmd.asyncResult(task, onSuccess, onError,
> onEnqueue)` instead — it folds the `Either` for you so the call site
> stays one expression. Future-level exceptions still surface as a
> `TermFlowErrorCmd` overlay automatically. See the [app-layer
> guide](../guide/app-layer.md#cmd--effects) for the full signature.

## 5. Starting a `Sub.Every`

The `Busy` handler decides whether the spinner needs to start:

```scala
case Busy(action) =>
  if sized.spinner.isActive then
    sized.copy(status = action).tui
  else
    sized.copy(
      status  = action,
      spinner = Sub.Every(200, () => SpinnerTick, ctx)
    ).tui
```

`Sub.Every(millis, () => Msg, ctx)` schedules a single-thread executor
that publishes `Cmd.GCmd(SpinnerTick)` to the bus every 200 ms. The
thunk re-evaluates each tick — useful when the message itself is
time-stamped or counter-keyed, although here `SpinnerTick` is constant.

Like `Sub.InputKey`, passing `ctx` auto-registers the sub for cleanup
when the runtime exits — you don't have to remember to cancel it on
`Exit` (though we will, defensively).

The `spinner.isActive` guard matters. If the user fires `Increment`
twice in quick succession, two `Busy` messages arrive. Without the
guard you'd start two timers and leak the first one.

## 6. Animating with `SpinnerTick`

Each tick advances the frame index modulo the frame count:

```scala
case SpinnerTick =>
  sized.copy(spinnerIndex = (sized.spinnerIndex + 1) % 4).tui
```

And in `view`:

```scala
private def statusWithSpinner(m: Model): String =
  val frames = Array("|", "/", "-", "\\")
  if m.spinner.isActive then s"${m.status} ${frames(m.spinnerIndex % frames.length)}"
  else m.status
```

The `m.spinner.isActive` check means the spinner glyph disappears the
instant we cancel the sub.

## 7. Stopping the spinner — `Sub.cancel`

When the future completes, `UpdateWith` runs:

```scala
case UpdateWith(c) =>
  if sized.spinner.isActive then sized.spinner.cancel()
  sized.copy(
    count        = c,
    status       = s"done::${TimeFormatter.getCurrentTime}",
    spinner      = Sub.NoSub,
    spinnerIndex = 0
  ).tui
```

Two things happen here:

- **`spinner.cancel()`** stops the executor. It's idempotent — calling
  it on an already-cancelled sub or on `Sub.NoSub` is safe.
- **`spinner = Sub.NoSub`** clears the model field so subsequent ticks
  (if any are in flight) become no-ops.

We also defensively cancel on `Exit`:

```scala
case Exit =>
  if sized.spinner.isActive then sized.spinner.cancel()
  Tui(sized, Cmd.Exit)
```

Strictly speaking the runtime would clean it up because `ctx`
auto-registered the sub, but explicit cancellation is a safe habit —
especially if you have multiple subs and want predictable shutdown
order.

## 8. Drawing the status line

The `view` is barely changed from Counter — we just slot
`statusWithSpinner(m)` into a new row:

```scala
TextNode(
  2.x, 3.y,
  List(Text(statusWithSpinner(m), Style(fg = Green)))
)
```

One subtle point: the spinner advances at 5 frames per second
(200 ms = 5 fps). That's deliberate — slow enough to be readable, fast
enough to feel alive. If you push it down to 50 ms you'll notice the
ANSI repaint flicker; if you push it up to 1 s it stops feeling
responsive.

## 9. Run it

```bash
sbt futureDemo
```

Type `increment`, hit Enter, and watch the `/` rotate for ~5 seconds
before the count updates. Try `decrement` next — that one waits ~10
seconds. Both can be queued back-to-back; the FCmd machinery handles
that correctly.

## What you've learned

- **`Cmd.FCmd`** is the bridge from `Future[A]` to the runtime. It
  takes the future, a result mapper to a follow-up `Cmd`, and an
  optional immediate `Msg` to dispatch at enqueue time.
- **`Sub.Every`** is the timer subscription. Auto-registers when given
  a `RuntimeCtx`; cancellable explicitly via `.cancel()`.
- **Sub fields on the model** keep async lifecycles addressable —
  always know which subs are live, store them so you can cancel them.
- **`Busy` / `UpdateWith` bracketing** is the standard pattern for
  signalling "work in progress" before the work itself completes.

Next up: [Forms and dialogs](04-forms-and-dialogs.md) — the Wizard
sample with multi-step focus management, validation, and modal
dialogs.
