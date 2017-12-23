package com.sos.jobscheduler.base.time

import io.circe
import java.time.Instant
import scala.math.abs
import scala.scalajs.js

/**
  * @author Joacim Zschimmer
  */
final case class Timestamp private(toEpochMilli: Long) extends GenericTimestamp[Timestamp] {

  def toIsoString = {
    val string = toJsDate.toISOString
    val sb = new StringBuilder(string)
    if (sb endsWith ".000Z") {
      sb.replace(sb.length - 5, sb.length, "Z")
      sb.toString
    }
      else string
  }
  //def toIsoString = toIsoStringBuilder.toString
  //
  //override def toIsoStringBuilder = {
  //  val sb = new StringBuilder(toJsDate.toISOString)
  //  if (sb endsWith ".000Z") {
  //    sb.replace(sb.length - 5, sb.length, "Z")
  //  }
  //  sb
  //}

  def toLocaleIsoString = {
    val date = toJsDate
    val offsetMinutes = date.getTimezoneOffset
    val offsetSuffix = f"${-offsetMinutes / 60}%+03d${abs(offsetMinutes) % 60}%02d"
    new js.Date((date.getTime - offsetMinutes*60000)).toISOString.stripSuffix("Z") + offsetSuffix
  }

  /** "2017-12-03T12:00:00.123" */
  def toLocaleIsoStringWithoutOffset = {
    val date = toJsDate
    val offsetMinutes = date.getTimezoneOffset
    new js.Date((date.getTime - offsetMinutes*60000)).toISOString.stripSuffix("Z")
  }

  /** "2017-12-03 12:00:00.123"
    * 'T' is replaced by space.
    * Trailing ".000" (zero millisecond) and ":00" (zero second) area stripped.
    */
  def toReadableLocaleIsoString = {
    val date = toJsDate
    val offsetMinutes = date.getTimezoneOffset
    val iso = new js.Date((date.getTime - offsetMinutes*60000)).toISOString
    iso.substring(0, 10) + " " + iso.substring(11)
      .stripSuffix(".000Z").stripSuffix("Z").stripSuffix(".000").stripSuffix(":00")
  }

  def toLocaleString = toJsDate.toLocaleString

  def toJsDate: js.Date =
    new js.Date(toEpochMilli)

  def copy(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli)
}

object Timestamp extends GenericTimestamp.Companion[Timestamp] {

  def ofEpochMilli(millis: Long) = new Timestamp(toEpochMilli = millis)

  def fromInstant(instant: Instant) = ofEpochMilli(instant.toEpochMilli)

  def parse(string: String) = ofEpochMilli((js.Date.parse(string)).toLong)

  def now: Timestamp = ofEpochMilli(epochMilli)

  def epochMilli: Long = js.Date.now.toLong

  implicit val JsonEncoder: circe.Encoder[Timestamp] =
    o ⇒ circe.Json.fromLong(o.toEpochMilli)

  implicit val JsonDecoder: circe.Decoder[Timestamp] =
    cursor ⇒
      cursor.as[Long] match {
        case Right(milli) ⇒ Right(ofEpochMilli(milli))
        case _ ⇒ cursor.as[String].map(parse)
      }
}
