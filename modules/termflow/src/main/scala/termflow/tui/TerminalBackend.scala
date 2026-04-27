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
   * [[Capabilities.detectFromEnv]]).
   */
  def capabilities: Capabilities = Capabilities.default

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

  override def close(): Unit =
    terminal.close()
