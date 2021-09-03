package js7.base.time

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import js7.base.time.JavaTime._
import js7.base.time.ScalaTime._
import scala.concurrent.duration.FiniteDuration

final case class LocalInterval(start: LocalDateTime, duration: FiniteDuration)
{
  def contains(local: LocalDateTime): Boolean =
    !start.isAfter(local) && end.isAfter(local)

  def endsBefore(local: LocalDateTime) =
    end <= local

  def end: LocalDateTime =
    start + duration

  def toTimeInterval(zone: ZoneId): TimeInterval =
    TimeInterval(
      JavaTimestamp.ofZoned(ZonedDateTime.of(start, zone)),
      duration)

  override def toString = s"LocalInterval($start, ${duration.pretty})"
}
