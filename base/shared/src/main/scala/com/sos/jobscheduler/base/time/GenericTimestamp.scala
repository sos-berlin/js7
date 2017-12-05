package com.sos.jobscheduler.base.time

import io.circe.{Decoder, Encoder}

/**
  * @author Joacim Zschimmer
  */
trait GenericTimestamp[A <: GenericTimestamp[A]] extends Ordered[A] {

  def toEpochMilli: Long

  def toEpochSecond = toEpochMilli / 1000

  def toIsoString: String

  //Problem with sbt: def toIsoStringBuilder: StringBuilder

  def compare(o: A) = toEpochMilli compare o.toEpochMilli

  override def toString = toIsoString
}

object GenericTimestamp {
  trait Companion[A <: GenericTimestamp[A]] {
    implicit val JsonEncoder: Encoder[A]
    implicit val JsonDecoder: Decoder[A]

    def apply(string: String): A =
      parse(string)

    def ofRoundedEpochMicros(o: Long) = ofEpochMilli(o / 1000)

    def ofEpochMilli(o: Long): A

    def ofEpochSecond(o: Long): A =
      ofEpochMilli(o * 1000)

    def parse(string: String): A

    def now: A
  }
}
