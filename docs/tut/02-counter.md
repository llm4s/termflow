# Counter

> *Stub — full content lands in Phase B of the docs roll-out.*

This tutorial will walk through `termflow.apps.counter.SyncCounter`, the
canonical "model + update + view" demo. The headline points it covers:

- Pattern-matching multiple `Msg` cases in `update`.
- Building a `Layout.Column` and resolving it at a coordinate.
- Reading a styled `Prompt` so users can type `increment` / `decrement`.

In the meantime, the source lives at
[`modules/termflow-sample/src/main/scala/termflow/apps/counter/SyncCounter.scala`](https://github.com/llm4s/termflow/blob/main/modules/termflow-sample/src/main/scala/termflow/apps/counter/SyncCounter.scala)
— launch with `sbt counterDemo` to see it run.
