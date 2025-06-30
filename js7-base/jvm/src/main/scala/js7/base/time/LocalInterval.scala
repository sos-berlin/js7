package js7.base.time

import cats.Eq
import java.time.{LocalDateTime, ZoneId, ZonedDateTime, Duration as JDuration}
import js7.base.time.JavaTime.extensions.*
import js7.base.time.ScalaTime.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.math.Ordered.orderingToOrdered

sealed trait LocalInterval extends Ordered[LocalInterval]:

  def contains(local: LocalDateTime)(using ZoneId): Boolean

  @TestOnly
  def contains(start: LocalDateTime, end: LocalDateTime)(using ZoneId): Boolean

  /** _ < start */
  def startsBefore(local: LocalDateTime)(using ZoneId): Boolean

  /** end < _ */
  def endsAfter(local: LocalDateTime)(using ZoneId): Boolean

  def isUnrestrictedStart(restriction: SchemeRestriction, dateOffset: JDuration): Boolean

  def toTimeInterval(using ZoneId): TimeInterval


object LocalInterval:
  implicit val eq: Eq[LocalInterval] = Eq.fromUniversalEquals

  def apply(start: LocalDateTime, duration: FiniteDuration): LocalInterval =
    Standard(start, duration)


  final case class Standard(start: LocalDateTime, duration: FiniteDuration)
  extends js7.base.time.LocalInterval:
    def contains(local: LocalDateTime)(using ZoneId): Boolean =
      // local should already be normalized, just to be sure:
      val normalizedLocal = local.normalize
      val normalizedStart = start.normalize
      !normalizedStart.isAfter(normalizedLocal)
        && (normalizedStart + duration).isAfter(normalizedLocal)

    def contains(start: LocalDateTime, end: LocalDateTime)(using ZoneId): Boolean =
      // start and end should already be normalized, just to be sure:
      val normalizedStart = start.normalize
      val normalizedEnd = end.normalize
      val normalizedThisStart = this.start.normalize
      normalizedThisStart < normalizedEnd
        && normalizedStart < (normalizedThisStart + duration)

    def startsBefore(local: LocalDateTime)(using ZoneId): Boolean =
      start.normalize < local.normalize

    def endsAfter(local: LocalDateTime)(using ZoneId): Boolean =
      local.normalize < start.normalize + duration

    def isUnrestrictedStart(restriction: SchemeRestriction, dateOffset: JDuration): Boolean =
      restriction.isUnrestricted(start, dateOffset)

    def toTimeInterval(using zoneId: ZoneId): TimeInterval =
      TimeInterval(
        JavaTimestamp.ofZoned(ZonedDateTime.of(start, zoneId)),
        duration)

    def compare(o: LocalInterval): Int =
      o match
        case Never => -1  // this < Never
        case Always => +1 // this > Always
        case o: Standard => start.compare(o.start)

    override def toString = s"LocalInterval($start, ${duration.pretty})"


  object Never extends LocalInterval:
    def contains(local: LocalDateTime)(using ZoneId) =
      false

    def contains(start: LocalDateTime, normalizedEnd: LocalDateTime)(using ZoneId) =
      false

    def startsBefore(local: LocalDateTime)(using ZoneId) =
      false

    def endsAfter(local: LocalDateTime)(using ZoneId) =
      false

    def isUnrestrictedStart(restriction: SchemeRestriction, dateOffset: JDuration): Boolean =
      false

    def toTimeInterval(using ZoneId): TimeInterval =
      TimeInterval.Never

    def compare(o: LocalInterval): Int =
      o match
        case Never => 0
        case _ => +1  // Never > Always, Never > Standard

    override def toString = "Never"


  object Always extends LocalInterval:
    def contains(local: LocalDateTime)(using ZoneId) =
      true

    def contains(start: LocalDateTime, end: LocalDateTime)(using ZoneId) =
      true

    def startsBefore(local: LocalDateTime)(using ZoneId) =
      true

    def endsAfter(local: LocalDateTime)(using ZoneId) =
      true

    def isUnrestrictedStart(restriction: SchemeRestriction, dateOffset: JDuration): Boolean =
      true // Restriction is not checked, because Always has no start !!!

    def toTimeInterval(using ZoneId): TimeInterval =
      TimeInterval.Always

    def compare(o: LocalInterval): Int =
      o match
        case Always => 0
        case _ => -1  // Always < 0, Always < Standard

    override def toString = "Always"
