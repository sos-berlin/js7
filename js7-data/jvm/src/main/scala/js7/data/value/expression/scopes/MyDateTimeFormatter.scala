package js7.data.value.expression.scopes

import java.time.format.DateTimeFormatter
import java.time.{OffsetDateTime, ZoneId}
import js7.base.problem.Checked
import js7.base.time.Timestamp

private object MyDateTimeFormatter
{
  def formatTimestamp(timestamp: Timestamp, format: String, maybeTimezone: Option[String])
  : Checked[String] =
    Checked.catchNonFatal {
      val zoneId = maybeTimezone.fold(ZoneId.systemDefault())(ZoneId.of)
      OffsetDateTime.ofInstant(timestamp.toInstant, zoneId).format(DateTimeFormatter.ofPattern(format))
    }
}
