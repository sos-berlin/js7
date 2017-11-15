package com.sos.jobscheduler.master.gui.data

import io.circe
import scala.math.abs
import scala.scalajs.js

/**
  * @author Joacim Zschimmer
  */
final case class Timestamp private(toEpochMilli: Long) {
  // JavaScript Date calculates with milliseconds

  override def toString = toIsoString

  def toIsoString = {
    val string = toJsDate.toISOString
    if (string endsWith ".000Z")
      string.dropRight(5) + "Z"
    else
      string
  }

  def toLocaleIsoString = {
    val date = toJsDate
    val offsetMinutes = date.getTimezoneOffset
    val offsetSuffix = f"${-offsetMinutes / 60}%+03d${abs(offsetMinutes) % 60}%02d"
    new js.Date((date.getTime - offsetMinutes*60000)).toISOString.stripSuffix("Z") + offsetSuffix
  }

  def toLocaleIsoStringWithoutOffset = {
    val date = toJsDate
    val offsetMinutes = date.getTimezoneOffset
    new js.Date((date.getTime - offsetMinutes*60000)).toISOString.stripSuffix("Z")
  }

  def toLocaleString = toJsDate.toLocaleString

  def toJsDate: js.Date =
    new js.Date(toEpochMilli)
}

object Timestamp {
  def fromEpochMilli(millis: Long) = new Timestamp(toEpochMilli = millis)

  def fromEpochMicro(micros: Long) = new Timestamp(toEpochMilli = micros / 1000)

  def apply(string: String) = fromEpochMilli((js.Date.parse(string)).toLong)

  implicit val jsonDecoder: circe.Decoder[Timestamp] =
    cursor ⇒
      cursor.as[Long] match {
        case Right(milli) ⇒ Right(fromEpochMilli(milli))
        case _ ⇒ cursor.as[String].map(apply)
      }
}
