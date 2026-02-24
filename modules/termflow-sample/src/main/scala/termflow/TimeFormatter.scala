package termflow

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object TimeFormatter:
  def getCurrentTime: String =
    val now       = ZonedDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss z")
    now.format(formatter)

  def getCustomFormat(format: String): String =
    val now       = ZonedDateTime.now()
    val formatter = DateTimeFormatter.ofPattern(format)
    now.format(formatter)

  def getReadableTime: String =
    getCustomFormat("MMMM dd, yyyy 'at' hh:mm:ss a z")

  def getISOTime: String =
    getCustomFormat("yyyy-MM-dd'T'HH:mm:ssXXX")
