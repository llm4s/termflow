package termflow.tui

import java.time.Instant
import scala.util.Try

final private[tui] class FrameworkLog private (logPath: LogPath):
  private def append(level: String, message: => String): Try[Unit] =
    logPath.appendUtf8Line(s"${Instant.now()} [$level] $message")

  def info(message: => String): Try[Unit] =
    append("INFO", message)

  def warn(message: => String): Try[Unit] =
    append("WARN", message)

  def error(message: => String): Try[Unit] =
    append("ERROR", message)

private[tui] object FrameworkLog:
  def apply(config: LoggingConfig): FrameworkLog =
    new FrameworkLog(config.path)
