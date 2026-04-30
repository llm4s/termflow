package termflow.tui

import org.jline.terminal.Terminal
import org.jline.terminal.TerminalBuilder

import java.io.InputStreamReader
import java.io.Reader
import java.io.Writer
import scala.jdk.CollectionConverters.*

/** Basic read-only terminal information. */
trait TerminalInfo:
  def width: Int
  def height: Int

/** Backend that provides terminal dimensions and a reader for raw key input. */
trait TerminalBackend extends TerminalInfo:
  def reader: Reader
  def writer: Writer
  def write(text: String): Unit = writer.write(text)
  def flush(): Unit             = writer.flush()
  def close(): Unit

  /**
   * Best-effort terminal capabilities. Defaults to a conservative
   * `Capabilities.default` (Ansi8 + Unicode on, mouse off); production
   * backends should override with a richer detection (see
   * [[Capabilities.detect]]).
   */
  def capabilities: Capabilities = Capabilities.default

  /**
   * Ring the terminal bell or pop the iTerm2 "request attention" indicator.
   *
   * Most terminals translate the BEL byte (`U+0007`) into a tab-bar activity
   * flag; tmux / screen translate it into a window-bell indicator (subject to
   * `monitor-bell`). iTerm2 also accepts an explicit `OSC 1337` request that
   * bounces the dock icon.
   *
   * No-ops when [[Capabilities.notifications]] is `Disabled`.
   *
   * The default trait implementation routes through
   * [[NotificationEscapes.attentionFor]]; tests can override.
   */
  def requestAttention(): Unit =
    NotificationEscapes.attentionFor(capabilities.notifications).foreach { seq =>
      writer.write(seq)
      writer.flush()
    }

  /**
   * Show a desktop notification (iTerm2 / kitty / VTE) or fall back to BEL.
   *
   * The body is interpreted as plain text. Carets, semicolons, and ESC bytes
   * inside `title` / `body` are stripped to avoid breaking the OSC sequence.
   *
   * No-ops when [[Capabilities.notifications]] is `Disabled`.
   */
  def notify(title: String, body: String): Unit =
    NotificationEscapes.notifyFor(capabilities.notifications, title, body).foreach { seq =>
      writer.write(seq)
      writer.flush()
    }

  /**
   * Register a listener to be invoked when the terminal is resized.
   *
   * Returns `Some(unregister)` if the backend supports event-based resize
   * notifications (e.g. SIGWINCH on a Unix tty); the caller must invoke
   * `unregister()` to remove the listener.
   *
   * Returns `None` if the backend has no signal mechanism — callers should
   * fall back to polling [[width]] / [[height]]. The default trait
   * implementation returns `None`.
   *
   * The listener is invoked from a backend-internal thread (e.g. the JVM
   * signal-dispatch thread) and should be cheap and non-blocking.
   */
  def onResize(listener: () => Unit): Option[() => Unit] = None

/** Default JLine-backed terminal implementation. */
final class JLineTerminalBackend extends TerminalBackend:
  private val terminal: Terminal =
    TerminalBuilder
      .builder()
      .system(true)
      .jna(true)
      .build()

  // Enter raw mode once; Sub / ConsoleKeyPressSource will read from this input stream.
  terminal.enterRawMode()

  private val input = terminal.input()

  override def reader: Reader =
    new InputStreamReader(input)

  override def writer: Writer =
    terminal.writer()

  override def width: Int  = terminal.getWidth
  override def height: Int = terminal.getHeight

  override val capabilities: Capabilities =
    Capabilities.detect(System.getenv().asScala.toMap)

  /**
   * Hook into JLine's SIGWINCH delivery. JLine raises [[Terminal.Signal.WINCH]]
   * when the underlying tty resizes (or, on Windows, when its emulator reports
   * a console-buffer change). The listener is invoked on JLine's signal
   * dispatch path; we adapt it through a Java [[Terminal.SignalHandler]] and
   * return the previous handler so callers can restore it on unregister.
   */
  override def onResize(listener: () => Unit): Option[() => Unit] =
    val handler = new Terminal.SignalHandler:
      override def handle(signal: Terminal.Signal): Unit =
        try listener()
        catch { case _: Throwable => () }
    val previous = terminal.handle(Terminal.Signal.WINCH, handler)
    Some { () =>
      terminal.handle(Terminal.Signal.WINCH, previous); ()
    }

  override def close(): Unit =
    terminal.close()
