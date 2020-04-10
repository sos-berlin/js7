package com.sos.jobscheduler.base.time

import com.sos.jobscheduler.base.time.Timestamp.dateTimeFormatter
import java.time.Instant
import java.time.format.DateTimeFormatter

/**
  * @author Joacim Zschimmer
  */
final case class Timestamp private(toEpochMilli: Long) extends GenericTimestamp[Timestamp]
{
  def companion = Timestamp

  /** Returns an ISO-8601 string with milliseconds.
    * For example "2017-12-04T11:22:33.456Z".
    */
  def toIsoString = dateTimeFormatter.format(toInstant)

  def toInstant = Instant.ofEpochMilli(toEpochMilli)

  def toJavaUtilDate: java.util.Date =
    new java.util.Date(toEpochMilli)

  def copy(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli)
}

object Timestamp extends GenericTimestamp.Companion[Timestamp]
{
  val MaxValue = ofEpochMilli(Long.MaxValue)

  def ofEpochMilli(o: Long) = new Timestamp(o)

  def apply(instant: Instant) = ofInstant(instant)

  def apply(date: java.util.Date) = ofJavaUtilDate(date)

  def ofInstant(instant: Instant) = new Timestamp(instant.toEpochMilli)

  def ofJavaUtilDate(date: java.util.Date) = Timestamp.ofEpochMilli(date.getTime)

  private def dateTimeFormatter = DateTimeFormatter.ISO_INSTANT

  def parse(string: String): Timestamp =
    ofInstant(Instant.from(dateTimeFormatter parse string))
}
