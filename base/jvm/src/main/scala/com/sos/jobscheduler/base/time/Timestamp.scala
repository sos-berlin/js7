package com.sos.jobscheduler.base.time

import com.sos.jobscheduler.base.time.Timestamp._
import java.time.Instant
import java.time.format.DateTimeFormatter

/**
  * @author Joacim Zschimmer
  */
final case class Timestamp private(toEpochMilli: Long) extends GenericTimestamp[Timestamp] {

  def toIsoString = dateTimeFormatter.format(toInstant)

  def toInstant = Instant.ofEpochMilli(toEpochMilli)

  def copy(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli)
}

object Timestamp extends GenericTimestamp.Companion[Timestamp] {

  def ofEpochMilli(o: Long) = new Timestamp(o)

  def ofInstant(instant: Instant) = new Timestamp(instant.toEpochMilli)

  private def dateTimeFormatter = DateTimeFormatter.ISO_INSTANT

  def parse(string: String): Timestamp =
    ofInstant(Instant.from(dateTimeFormatter parse string))

  def now: Timestamp = ofEpochMilli(epochMilli)

  def epochMilli: Long = System.currentTimeMillis
}
