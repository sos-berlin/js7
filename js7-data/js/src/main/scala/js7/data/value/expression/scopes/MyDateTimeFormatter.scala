package js7.data.value.expression.scopes

import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp

private object MyDateTimeFormatter
{
  def formatTimestamp(timestamp: Timestamp, format: String, maybeTimezone: Option[String])
  : Checked[String] =
    Left(Problem("DateTimeFormatter is not available for Scala.js"))
}
