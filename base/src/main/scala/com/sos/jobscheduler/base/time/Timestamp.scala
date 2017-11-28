package com.sos.jobscheduler.base.time

import com.sos.jobscheduler.base.generic.GenericLong
import java.time.Instant

/**
  * @author Joacim Zschimmer
  */
final case class Timestamp(number: Long) extends GenericLong with Ordered[Timestamp] {

  def toEpochMilli = number

  def compare(o: Timestamp) = number compare o.number
}

object Timestamp extends GenericLong.Companion[Timestamp] {

  def ofEpochMilli(o: Long) = Timestamp(o)

  def ofEpochSecond(o: Long) = Timestamp(o * 1000)

  def parse(string: String) = ofEpochMilli(Instant.parse(string).toEpochMilli)

  def now = ofEpochMilli(System.currentTimeMillis)

  //implicit val JsonEncoder: Encoder[Timestamp] = o ⇒ Json.fromLong(o.toMillis)
  //
  //implicit val JsonDecoder: Decoder[Timestamp] = _.value.asNumber.flatMap(_.toLong).map(Timestamp.apply) match {
  //  case Some(o) ⇒ Right(o)
  //  case None ⇒ Left(throw new RuntimeException("JSON Timestamp expected"))
  //}
}
