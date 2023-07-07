package js7.base.time

import java.time.{LocalDate, LocalDateTime, ZoneId}
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.concurrent.duration.FiniteDuration

/** Adaptor AdmissionTimeScheme to Java Time.*/
object AdmissionTimeSchemeForJavaTime
{
  implicit final class RichAdmissionTimeScheme(private val admissionTimeScheme: AdmissionTimeScheme)
  extends AnyVal
  {
    def hasAdmissionPeriodStartForDay(localDate: LocalDate, dateOffset: FiniteDuration): Boolean =
      admissionTimeScheme.periods
        .view
        .map(AdmissionPeriodCalculator(_, dateOffset))
        .exists(_.hasAdmissionPeriodStartForDay(localDate))

    def isPermitted(timestamp: Timestamp, zone: ZoneId, dateOffset: FiniteDuration): Boolean =
      findTimeInterval(timestamp, zone, dateOffset)
        .exists(_.contains(timestamp))

    /** Find current and next TimeIntervals until `until` for admission.
     * Shifts the interval when daylight saving time skips an hour. */
    def findTimeIntervals(
      from: Timestamp, until: Timestamp, zone: ZoneId, dateOffset: FiniteDuration)
    : View[(Int, TimeInterval)] =
      findLocalIntervals(from.toLocalDateTime(zone), dateOffset)
        .map { case (periodIndex, localInterval) =>
          periodIndex -> localInterval.toTimeInterval(zone) }
        .takeWhile(_._2.startsBefore(until))
        .filterNot(_._2.endsBefore(from))

    /** Find a current or next TimeInterval for admission.
     * Shifts the interval when daylight saving time skips an hour. */
    def findTimeInterval(from: Timestamp, zone: ZoneId, dateOffset: FiniteDuration)
    : Option[TimeInterval] =
      findLocalIntervals(from.toLocalDateTime(zone), dateOffset)
        .map(_._2)
        .map(_.toTimeInterval(zone))
        .filterNot(_.endsBefore(from))
        .headOption

    /** Calculates end time with local time, yielding an hour more or less when dst shifts. */
    @TestOnly
    def findLocalInterval(from: LocalDateTime, dateOffset: FiniteDuration): Option[LocalInterval] =
      findLocalIntervals(from, dateOffset)
        .map(_._2)
        .filterNot(_.endsBefore(from))
        .headOption

    /** Return View[(period index, LocalInterval)]). */
    private[time] def findLocalIntervals(local: LocalDateTime, dateOffset: FiniteDuration)
    : View[(Int, LocalInterval)] =
      View.fromIteratorProvider(() => admissionTimeScheme.periods
        .zipWithIndex
        .map { case (period, i) =>
          AdmissionPeriodCalculator(period, dateOffset).findLocalIntervals(local).map(i -> _)
        }
        .mergeOrderedBy(_._2))
  }
}
