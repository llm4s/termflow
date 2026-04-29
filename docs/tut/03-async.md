# Async work

> *Stub — full content lands in Phase B of the docs roll-out.*

This tutorial will cover `Cmd.FCmd` and `Sub.Every`, building on
[`termflow.apps.counter.FutureCounter`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/counter/FutureCounter.scala)
— a counter where increment/decrement are asynchronous and a spinner
keeps drawing while the work is in flight.

Headline points it will cover:

- Returning `Cmd.FCmd[A, Msg]` from `update` to fire async work.
- Wrapping the result back into a `Msg` so `update` stays pure.
- Using `Sub.Every` to drive a spinner on a fixed cadence.
- Cancelling subscriptions cleanly on `Cmd.Exit`.

For now, run it with `sbt futureDemo`.
