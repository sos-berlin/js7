package js7.base.time

import cats.Eq
import java.time.{LocalDateTime, ZoneId, ZonedDateTime, Duration as JDuration}
import js7.base.time.JavaTime.extensions.*
import js7.base.time.ScalaTime.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.math.Ordered.orderingToOrdered

final case class LocalInterval(start: LocalDateTime, duration: FiniteDuration):
  // TODO Does not respect daylight-saving time.
  // In case, the interval crosses a daylight-saving time boundary,
  // the end time is shifted by an hour.
  private[time] def end: LocalDateTime =
    start.plusNanos(duration.toNanos)

  def contains(local: LocalDateTime)(using ZoneId): Boolean =
    // local should already be normalized, just to be sure:
    val normalizedLocal = local.normalize
    val normalizedStart = start.normalize
    !normalizedStart.isAfter(normalizedLocal)
      && (normalizedStart + duration).isAfter(normalizedLocal)

  @TestOnly
  def contains(start: LocalDateTime, end: LocalDateTime)(using ZoneId): Boolean =
    // start and end should already be normalized, just to be sure:
    val normalizedStart = start.normalize
    val normalizedEnd = end.normalize
    val normalizedThisStart = this.start.normalize
    normalizedThisStart < normalizedEnd
      && normalizedStart < (normalizedThisStart + duration)

  /** _ < start */
  def startsBefore(local: LocalDateTime)(using ZoneId): Boolean =
    start.normalize < local.normalize

  /** end < _ */
  def endsAfter(local: LocalDateTime)(using ZoneId): Boolean =
    local.normalize < start.normalize + duration

  def clip(restriction: SchemeRestriction, dateOffset: JDuration): Option[LocalInterval] =
    restriction.clipLocalInterval(this, dateOffset)

  def toTimeInterval(using zoneId: ZoneId): TimeInterval.Standard =
    TimeInterval.Standard(
      JavaTimestamp.ofZoned(ZonedDateTime.of(start, zoneId)),
      duration)

  def compare(o: LocalInterval): Int =
    start.compare(o.start)

  override def toString =
    s"LocalInterval($start, ${duration.pretty})"


object LocalInterval:
  given Eq[LocalInterval] = Eq.fromUniversalEquals

  given Ordering[LocalInterval] = Ordering.by(_.start)

  private def startOfNextMonth(dt: LocalDateTime, dateOffset: JDuration): LocalDateTime =
    dt.minus(dateOffset).toLocalDate
      .plusMonths(1).withDayOfMonth(1)
      .atStartOfDay
      .plus(dateOffset)
