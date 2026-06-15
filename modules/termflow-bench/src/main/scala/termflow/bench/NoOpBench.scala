package termflow.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

/**
 * Trivial smoke benchmark proving the JMH harness compiles and runs.
 *
 * It does no TermFlow work; it only sums a small range of ints into a
 * [[Blackhole]] so the JIT cannot elide the loop. Real benchmarks land in
 * sibling classes; this one exists so `sbt "bench/Jmh/run"` has something to
 * execute from day one.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class NoOpBench:

  @Benchmark
  def sumInts(bh: Blackhole): Unit =
    var total = 0
    var i     = 0
    while i < 1000 do
      total += i
      i += 1
    bh.consume(total)
