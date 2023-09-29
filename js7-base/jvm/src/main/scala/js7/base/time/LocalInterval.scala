package js7.base.time

import cats.Eq
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import js7.base.time.JavaTime.*
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.FiniteDuration

sealed trait LocalInterval extends Ordered[LocalInterval]:
  def contains(local: LocalDateTime): Boolean

  def contains(start: LocalDateTime, end: LocalDateTime): Boolean

  /** x <= start. */
  def startsBefore(local: LocalDateTime): Boolean

  /** x <= end. */
  def endsBefore(local: LocalDateTime): Boolean

  def toTimeInterval(zone: ZoneId): TimeInterval

object LocalInterval:
  implicit val eq: Eq[LocalInterval] = Eq.fromUniversalEquals

  def apply(start: LocalDateTime, duration: FiniteDuration): LocalInterval =
    Standard(start, duration)

  final case class Standard(start: LocalDateTime, duration: FiniteDuration)
  extends js7.base.time.LocalInterval:
    def contains(local: LocalDateTime): Boolean =
      !start.isAfter(local) && end.isAfter(local)

    def contains(start: LocalDateTime, end: LocalDateTime) =
      this.start < end && start < this.end

    def startsBefore(local: LocalDateTime) =
      start <= local

    def endsBefore(local: LocalDateTime) =
      end <= local

    def end: LocalDateTime =
      start + duration

    def toTimeInterval(zone: ZoneId): TimeInterval =
      TimeInterval(
        JavaTimestamp.ofZoned(ZonedDateTime.of(start, zone)),
        duration)

    def compare(o: LocalInterval) =
      o match
        case Never => -1  // this < Never
        case Always => +1 // this > Always
        case o: Standard => start.compare(o.start)

    override def toString = s"LocalInterval($start, ${duration.pretty})"

  object Never extends LocalInterval:
    def contains(local: LocalDateTime) =
      false

    def contains(start: LocalDateTime, end: LocalDateTime) =
      false

    def startsBefore(local: LocalDateTime) =
      false

    def endsBefore(local: LocalDateTime) =
      true

    def toTimeInterval(zone: ZoneId) =
      TimeInterval.Never

    def compare(o: LocalInterval) =
      o match
        case Never => 0
        case _ => +1  // Never > Always, Never > Standard

    override def toString = "Never"

  object Always extends LocalInterval:
    def contains(local: LocalDateTime) =
      true

    def contains(start: LocalDateTime, end: LocalDateTime) =
      true

    def startsBefore(local: LocalDateTime) =
      true

    def endsBefore(local: LocalDateTime) =
      false

    def toTimeInterval(zone: ZoneId) =
      TimeInterval.Always

    def compare(o: LocalInterval) =
      o match
        case Always => 0
        case _ => -1  // Always < 0, Always < Standard

    override def toString = "Always"
