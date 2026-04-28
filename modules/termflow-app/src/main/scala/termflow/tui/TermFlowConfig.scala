package termflow.tui

import pureconfig.ConfigReader
import pureconfig.ConfigSource

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import scala.util.Try

opaque type LogPath = Path

object LogPath:
  def apply(path: Path): LogPath =
    path

  extension (logPath: LogPath)
    def path: Path =
      logPath

    def appendUtf8Line(line: String): Try[Unit] =
      Try:
        parentOption.foreach(Files.createDirectories(_): Unit)
        Files.write(
          logPath,
          (line + System.lineSeparator()).getBytes(StandardCharsets.UTF_8),
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        ): Unit

    private def parentOption: Option[Path] =
      Option(logPath.getParent)

final case class TermFlowConfig(
  logging: LoggingConfig,
  metrics: MetricsConfig
)

object TermFlowConfig:
  def load(): Try[TermFlowConfig] =
    TermFlowConfigLoader.load()

final case class LoggingConfig(path: LogPath)

final case class MetricsConfig(enabled: Boolean)

private[tui] object TermFlowConfigLoader:
  final private case class RawTermFlowConfig(
    logging: RawLoggingConfig,
    metrics: RawMetricsConfig
  ) derives ConfigReader

  final private case class RawLoggingConfig(path: Path) derives ConfigReader

  final private case class RawMetricsConfig(
    enabled: Boolean,
    envOverride: Option[String] = None
  ) derives ConfigReader

  def load(): Try[TermFlowConfig] =
    loadFrom(ConfigSource.default.at("termflow"))

  private[tui] def loadFrom(source: ConfigSource): Try[TermFlowConfig] =
    source
      .load[RawTermFlowConfig]
      .fold(
        failures => scala.util.Failure(IllegalStateException(failures.prettyPrint())),
        raw =>
          scala.util.Success(
            TermFlowConfig(
              logging = LoggingConfig(path = LogPath(raw.logging.path)),
              metrics = MetricsConfig(enabled = resolveMetricsEnabled(raw.metrics))
            )
          )
      )

  private def resolveMetricsEnabled(raw: RawMetricsConfig): Boolean =
    raw.envOverride
      .map(parseBoolean)
      .getOrElse(raw.enabled)

  private def parseBoolean(value: String): Boolean =
    value.nonEmpty && value != "0" && !value.equalsIgnoreCase("false")
