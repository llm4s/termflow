package termflow.testkit

import termflow.tui.Cmd
import termflow.tui.LogPath
import termflow.tui.LoggingConfig
import termflow.tui.MetricsConfig
import termflow.tui.RuntimeCtx
import termflow.tui.Sub
import termflow.tui.TermFlowConfig
import termflow.tui.TerminalBackend

import java.io.Reader
import java.io.StringReader
import java.io.StringWriter
import java.io.Writer
import java.nio.file.Path
import scala.collection.mutable

/**
 * Stub `RuntimeCtx` for snapshot tests.
 *
 * Captures published commands into an in-memory buffer and records any
 * subscriptions that apps register during `init`/`update`, but deliberately
 * does NOT drive subscription threads — timers, input readers, and resize
 * polls stay dormant so tests remain fully deterministic.
 */
final class TestRuntimeCtx[Msg](
  override val terminal: TerminalBackend,
  override val config: TermFlowConfig
) extends RuntimeCtx[Msg]:

  private val lock      = new Object
  private val cmdBuffer = mutable.Queue.empty[Cmd[Msg]]
  private val subs      = mutable.ArrayBuffer.empty[Sub[Msg]]

  override def publish(cmd: Cmd[Msg]): Unit =
    lock.synchronized {
      val _ = cmdBuffer.enqueue(cmd)
    }

  override def registerSub(sub: Sub[Msg]): Sub[Msg] =
    lock.synchronized {
      val _ = subs.append(sub)
      sub
    }

  /** Remove and return every command queued so far. */
  def drainCmds(): List[Cmd[Msg]] =
    lock.synchronized {
      val out = cmdBuffer.toList
      cmdBuffer.clear()
      out
    }

  /** Snapshot of subscriptions registered so far (read-only view). */
  def registeredSubs: List[Sub[Msg]] =
    lock.synchronized(subs.toList)

  /** Cancel every registered subscription, swallowing errors. */
  def cancelSubs(): Unit =
    lock.synchronized {
      subs.foreach { s =>
        try s.cancel()
        catch { case _: Throwable => () }
      }
      subs.clear()
    }

object TestRuntimeCtx:

  /** Minimal `TermFlowConfig` suitable for tests — logging to target/, metrics off. */
  val TestConfig: TermFlowConfig =
    TermFlowConfig(
      logging = LoggingConfig(LogPath(Path.of("target", "termflow-testkit.log"))),
      metrics = MetricsConfig(enabled = false)
    )

  /** Build a `TestRuntimeCtx` wired up to a fake terminal of the given dimensions. */
  def apply[Msg](width: Int, height: Int): TestRuntimeCtx[Msg] =
    new TestRuntimeCtx[Msg](
      terminal = new TestTerminalBackend(width, height),
      config = TestConfig
    )

  /**
   * Fake `TerminalBackend` with fixed dimensions and an empty reader.
   *
   * An empty `StringReader` causes any `Sub.InputKey` registered during
   * `init` to see end-of-stream immediately, so the input thread spawned by
   * `ConsoleKeyPressSource` exits without publishing anything.
   */
  final class TestTerminalBackend(val width: Int, val height: Int) extends TerminalBackend:
    private val out             = new StringWriter()
    override def reader: Reader = new StringReader("")
    override def writer: Writer = out
    override def close(): Unit  = ()
