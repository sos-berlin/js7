package js7.base.time

import js7.base.problem.Problem
import scala.math.abs
import scala.scalajs.js

/**
  * @author Joacim Zschimmer
  */
final case class JsTimestamp private(toEpochMilli: Long) extends Timestamp
{
  import JsTimestamp.specific.*

  def companion = JsTimestamp

  /** "2017-12-04T11:22:33.456Z". */
  def toIsoString = {
    val string = this.toJsDate.toISOString()
    if (string endsWith ".000Z") {
      val sb = new StringBuilder(string)
      sb.replace(sb.length - 5, sb.length, "Z")
      sb.toString
    } else
      string
  }

  def format(format: String, maybeTimezone: Option[String]) =
    Left(Problem("Date formatting is not available for Scala.js"))

  def copy(epochMilli: Long) =
    JsTimestamp.ofEpochMilli(epochMilli)
}


object JsTimestamp extends Timestamp.Companion
{
  object specific {
    implicit class RichJsTimestamp(private val timestamp: Timestamp) extends AnyVal
    {
      /** "2017-12-03T12:00:00.123" */
      def toLocaleIsoStringWithoutOffset = {
        val date = toJsDate
        val offsetMinutes = date.getTimezoneOffset().toInt
        new js.Date(date.getTime() - offsetMinutes * 60000).toISOString().stripSuffix("Z")
      }

      /** "2017-12-03 12:00:00.123"
        * 'T' is replaced by space.
        * Trailing ".000" (zero millisecond) and ":00" (zero second) area stripped.
        */
      def toReadableLocaleIsoString = {
        val date = toJsDate
        val offsetMinutes = date.getTimezoneOffset().toInt
        val iso = new js.Date(date.getTime() - offsetMinutes * 60000).toISOString()
        iso.substring(0, 10) + " " + iso.substring(11)
          .stripSuffix(".000Z").stripSuffix("Z").stripSuffix(".000").stripSuffix(":00")
      }

      def toLocaleIsoString = {
        val date = toJsDate
        val offsetMinutes = date.getTimezoneOffset().toInt
        val offsetSuffix = f"${-offsetMinutes / 60}%+03d${abs(offsetMinutes) % 60}%02d"
        new js.Date(date.getTime() - offsetMinutes * 60000).toISOString().stripSuffix("Z") + offsetSuffix
      }

      def toLocaleString = toJsDate.toLocaleString()

      def toJsDate: js.Date =
        new js.Date(timestamp.toEpochMilli.toDouble)

    }
  }

  def ofEpochMilli(millis: Long) = new SystemTimestamp(toEpochMilli = millis)

  def parse(string: String) = ofEpochMilli(js.Date.parse(string).toLong)
}
