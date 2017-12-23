package com.sos.jobscheduler.base.time

import io.circe
import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
final case class Timestamp private(toEpochMilli: Long) extends GenericTimestamp[Timestamp] {

  def toIsoString = toInstant.toString

  //def toIsoStringBuilder = new StringBuilder(toIsoString)

  def toInstant = Instant.ofEpochMilli(toEpochMilli)

  def copy(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli)
}

object Timestamp extends GenericTimestamp.Companion[Timestamp] {

  def ofEpochMilli(o: Long) = new Timestamp(o)

  def parse(string: String) = ofEpochMilli(Instant.parse(string).toEpochMilli)

  def now: Timestamp = ofEpochMilli(epochMilli)

  def epochMilli: Long = System.currentTimeMillis

  implicit val JsonEncoder: circe.Encoder[Timestamp] =
    o ⇒ circe.Json.fromLong(o.toEpochMilli)

  implicit val JsonDecoder: circe.Decoder[Timestamp] =
    cursor ⇒
      cursor.as[Long] match {
        case Right(milli) ⇒ Right(ofEpochMilli(milli))
        case _ ⇒ cursor.as[String].map(parse)
      }
}
