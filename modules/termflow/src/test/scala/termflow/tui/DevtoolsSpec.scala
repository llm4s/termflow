package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class DevtoolsSpec extends AnyFunSuite:

  enum DemoMsg:
    case Inc, Dec, Reset

  // --- construction ---------------------------------------------------------

  test("History.empty starts with size 0 and isEmpty true"):
    val h = Devtools.History.empty[DemoMsg, Int](capacity = 8)
    assert(h.isEmpty)
    assert(h.size == 0)
    assert(!h.isFull)
    assert(h.latest.isEmpty)
    assert(h.oldest.isEmpty)

  test("History rejects non-positive capacity"):
    intercept[IllegalArgumentException](Devtools.History.empty[DemoMsg, Int](capacity = 0))
    intercept[IllegalArgumentException](Devtools.History.empty[DemoMsg, Int](capacity = -1))

  // --- snapshot / record ----------------------------------------------------

  test("snapshot adds an initial frame with no message"):
    val h = Devtools.History.empty[DemoMsg, Int](capacity = 4).snapshot(0, atMillis = 100L)
    assert(h.size == 1)
    val f = h.latest.get
    assert(f.index == 0)
    assert(f.timestampMillis == 100L)
    assert(f.msg.isEmpty)
    assert(f.model == 0)

  test("record adds a transition frame with the dispatched message"):
    val h = Devtools.History
      .empty[DemoMsg, Int](capacity = 4)
      .snapshot(0, atMillis = 100L)
      .record(DemoMsg.Inc, 1, atMillis = 200L)
    assert(h.size == 2)
    val f = h.latest.get
    assert(f.index == 1)
    assert(f.timestampMillis == 200L)
    assert(f.msg.contains(DemoMsg.Inc))
    assert(f.model == 1)

  test("indices increment monotonically across snapshot and record"):
    val h = Devtools.History
      .empty[DemoMsg, Int](capacity = 8)
      .snapshot(0, atMillis = 0L)
      .record(DemoMsg.Inc, 1, atMillis = 0L)
      .record(DemoMsg.Inc, 2, atMillis = 0L)
      .record(DemoMsg.Dec, 1, atMillis = 0L)
    assert(h.frames.map(_.index) == Vector(0, 1, 2, 3))

  // --- ring-buffer wrap ----------------------------------------------------

  test("recording past capacity evicts the oldest frame"):
    val h0 = Devtools.History.empty[DemoMsg, Int](capacity = 3)
    val h  = (1 to 5).foldLeft(h0)((acc, n) => acc.record(DemoMsg.Inc, n, atMillis = n.toLong))
    assert(h.size == 3)
    assert(h.isFull)
    // Oldest two frames (indices 0 and 1) were evicted.
    assert(h.frames.map(_.index) == Vector(2, 3, 4))
    assert(h.frames.map(_.model) == Vector(3, 4, 5))
    // nextIndex continues unaffected.
    assert(h.nextIndex == 5)

  test("at(index) returns the frame for retained indices and None for evicted ones"):
    val h0 = Devtools.History.empty[DemoMsg, Int](capacity = 3)
    val h  = (0 to 4).foldLeft(h0)((acc, n) => acc.record(DemoMsg.Inc, n, atMillis = n.toLong))
    // Indices 0 and 1 evicted; 2..4 retained.
    assert(h.at(0).isEmpty)
    assert(h.at(1).isEmpty)
    assert(h.at(2).map(_.model).contains(2))
    assert(h.at(4).map(_.model).contains(4))
    assert(h.at(99).isEmpty)

  // --- rewindTo ------------------------------------------------------------

  test("rewindTo truncates to the named frame and resets nextIndex"):
    val h0 = (0 to 4).foldLeft(Devtools.History.empty[DemoMsg, Int](capacity = 8))((acc, n) =>
      acc.record(DemoMsg.Inc, n, atMillis = 0L)
    )
    val rewound = h0.rewindTo(2).get
    assert(rewound.frames.map(_.index) == Vector(0, 1, 2))
    assert(rewound.nextIndex == 3)
    // A new record after rewind continues with the next index.
    val branched = rewound.record(DemoMsg.Reset, 99, atMillis = 0L)
    assert(branched.latest.get.index == 3)
    assert(branched.latest.get.msg.contains(DemoMsg.Reset))
    assert(branched.latest.get.model == 99)

  test("rewindTo returns None for indices not present in the window"):
    val h0 = Devtools.History.empty[DemoMsg, Int](capacity = 2)
    val h  = h0.record(DemoMsg.Inc, 1, atMillis = 0L).record(DemoMsg.Inc, 2, atMillis = 0L)
    // Frame at index 0 was evicted; rewindTo(0) should fail.
    val h2 = h.record(DemoMsg.Inc, 3, atMillis = 0L)
    assert(h2.rewindTo(0).isEmpty)
    // Future indices likewise.
    assert(h2.rewindTo(99).isEmpty)

  test("rewindTo to the latest index is a no-op"):
    val h = Devtools.History
      .empty[DemoMsg, Int](capacity = 4)
      .record(DemoMsg.Inc, 1, atMillis = 0L)
      .record(DemoMsg.Inc, 2, atMillis = 0L)
    val rewound = h.rewindTo(1).get
    assert(rewound.frames == h.frames)
    assert(rewound.nextIndex == 2)

  // --- cleared --------------------------------------------------------------

  test("cleared empties frames but preserves capacity and nextIndex"):
    val h = Devtools.History
      .empty[DemoMsg, Int](capacity = 8)
      .record(DemoMsg.Inc, 1, atMillis = 0L)
      .record(DemoMsg.Inc, 2, atMillis = 0L)
    val cleared = h.cleared
    assert(cleared.isEmpty)
    assert(cleared.capacity == 8)
    assert(cleared.nextIndex == 2)
    // Subsequent records get fresh, non-colliding indices.
    val next = cleared.record(DemoMsg.Reset, 0, atMillis = 0L)
    assert(next.latest.get.index == 2)

  // --- toReport ------------------------------------------------------------

  test("toReport on an empty history returns a placeholder"):
    val r = Devtools.History.empty[DemoMsg, Int](capacity = 4).toReport
    assert(r == "(no recorded frames)")

  test("toReport renders the header and one line per frame with relative timestamps"):
    val h = Devtools.History
      .empty[DemoMsg, Int](capacity = 8)
      .snapshot(0, atMillis = 1000L)
      .record(DemoMsg.Inc, 1, atMillis = 1120L)
      .record(DemoMsg.Inc, 2, atMillis = 1300L)
    val r = h.toReport
    assert(r.contains("3/8 frames retained"))
    // Timestamps are relative to the first retained frame.
    assert(r.contains("t=+    0ms"))
    assert(r.contains("t=+  120ms"))
    assert(r.contains("t=+  300ms"))
    // Initial snapshot is labelled as such; transitions show their msg.
    assert(r.contains("(initial)"))
    assert(r.contains("Inc"))

  test("toReport truncates very long msg / model strings"):
    case class BigModel(label: String)
    val long = "a" * 200
    val h = Devtools.History
      .empty[String, BigModel](capacity = 4)
      .record(long, BigModel(long), atMillis = 0L)
    val r = h.toReport
    // Truncation marker '…' should appear for both columns.
    assert(r.contains("…"))

  // --- immutability ---------------------------------------------------------

  test("transitions never mutate the receiver"):
    val h0 = Devtools.History.empty[DemoMsg, Int](capacity = 4).snapshot(0, atMillis = 0L)
    val h1 = h0.record(DemoMsg.Inc, 1, atMillis = 0L)
    val h2 = h0.record(DemoMsg.Dec, -1, atMillis = 0L)
    // Both branches see the same h0.
    assert(h0.size == 1)
    assert(h0.latest.get.model == 0)
    // The two branches diverge.
    assert(h1.latest.get.model == 1)
    assert(h2.latest.get.model == -1)
