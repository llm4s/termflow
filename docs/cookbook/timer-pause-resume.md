# Pause and resume a `Sub.Every` timer

`Sub.Every` has no native pause/resume — `cancel()` is final. To
"resume" you create a new subscription. This is fine because
subscription construction is cheap and `Sub.NoSub` is a safe
placeholder.

## Pattern

1. Store the timer `Sub[Msg]` on your model — `Sub.NoSub` when
   inactive.
2. To start: replace the field with `Sub.Every(...)`.
3. To pause: `cancel()` the existing sub and write `Sub.NoSub` back.
4. To resume: build a fresh `Sub.Every` and store it.

## Code

```scala
import termflow.tui.{Cmd, Sub, Tui}
import termflow.tui.Tui.*

final case class Model(
  ticks:     Int,
  ticker:    Sub[Msg],    // Sub.NoSub when paused
  intervalMs: Long
)

enum Msg:
  case Tick
  case Pause
  case Resume
  case SetInterval(ms: Long)
  case Quit

def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
  msg match
    case Msg.Tick =>
      m.copy(ticks = m.ticks + 1).tui

    case Msg.Pause =>
      if m.ticker.isActive then m.ticker.cancel()
      m.copy(ticker = Sub.NoSub).tui

    case Msg.Resume =>
      if m.ticker.isActive then m.tui   // already running
      else m.copy(ticker = Sub.Every(m.intervalMs, () => Msg.Tick, ctx)).tui

    case Msg.SetInterval(ms) =>
      // Changing interval = cancel + restart with the new rate.
      if m.ticker.isActive then m.ticker.cancel()
      m.copy(
        intervalMs = ms,
        ticker     = Sub.Every(ms, () => Msg.Tick, ctx)
      ).tui

    case Msg.Quit =>
      if m.ticker.isActive then m.ticker.cancel()
      Tui(m, Cmd.Exit)
```

## Notes

- **`isActive` check before `cancel()`** is paranoia — `cancel()` is
  idempotent, but the guard makes the intent explicit.
- **`Sub.Every` auto-registers via `ctx`** so the runtime cancels it
  on `Cmd.Exit` regardless of whether you remembered to. The explicit
  cancel above is defensive.
- **Don't capture `m.intervalMs` at sub construction** — pass it
  through. The thunk `() => Msg.Tick` re-evaluates per tick, but the
  `millis` argument is fixed for the sub's lifetime.
- **Multiple timers** don't share a thread — each `Sub.Every` spins up
  its own `ScheduledExecutorService`. Cheap, but worth knowing if
  you're building a thousand-timer app.

For a working example, see the *Async work* tutorial — the
`FutureCounter` spinner uses exactly this pattern, except the start /
stop boundary is "async work in flight" rather than user-driven
pause.
