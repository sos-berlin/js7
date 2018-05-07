package com.sos.jobscheduler.base.time

import io.circe
import java.util.concurrent.TimeUnit.MILLISECONDS
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * @author Joacim Zschimmer
  */
trait GenericTimestamp[A <: GenericTimestamp[A]] extends Ordered[A] {

  def toEpochMilli: Long

  def toEpochSecond = toEpochMilli / 1000

  def toIsoString: String

  //Problem with sbt: def toIsoStringBuilder: StringBuilder

  def compare(o: A) = toEpochMilli compare o.toEpochMilli

  def +(o: Duration) = copy(epochMilli = toEpochMilli + o.toMillis)

  def -(o: GenericTimestamp[A]) = new FiniteDuration(toEpochMilli - o.toEpochMilli, MILLISECONDS)

  def copy(epochMilli: Long): A

  override def toString = toIsoString
}

object GenericTimestamp {
  trait Companion[A <: GenericTimestamp[A]] {
    val StringTimestampJsonEncoder: circe.Encoder[A] =
      o ⇒ circe.Json.fromString(o.toIsoString)

    val NumericTimestampJsonEncoder: circe.Encoder[A] =
      o ⇒ circe.Json.fromLong(o.toEpochMilli)

    implicit val jsonEncoder: circe.Encoder[A] = NumericTimestampJsonEncoder

    implicit val jsonDecoder: circe.Decoder[A] =
      cursor ⇒
        if (cursor.value.isNumber)
          cursor.as[Long] map ofEpochMilli
        else
          cursor.as[String] map parse

    def apply(string: String): A =
      parse(string)

    def ofEpochMilli(o: Long): A

    def ofEpochSecond(o: Long): A =
      ofEpochMilli(o * 1000)

    def parse(string: String): A

    def now: A
  }
}
