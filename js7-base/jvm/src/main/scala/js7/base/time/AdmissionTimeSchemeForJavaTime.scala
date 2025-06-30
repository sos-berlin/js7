package js7.base.time

import java.time.{LocalDate, LocalDateTime, ZoneId}
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.concurrent.duration.FiniteDuration

/** Adaptor AdmissionTimeScheme to Java Time.*/
object AdmissionTimeSchemeForJavaTime:

  implicit final class RichAdmissionTimeScheme(private val admissionTimeScheme: AdmissionTimeScheme)
  extends AnyVal:
    def hasAdmissionPeriodStartForDay(localDate: LocalDate, dateOffset: FiniteDuration)
      (using ZoneId)
    : Boolean =
      admissionTimeScheme.restrictedSchemes
        .view
        .flatMap: restrictedScheme =>
          restrictedScheme.periods.map(restrictedScheme.restriction -> _)
        .map: (restriction, period) =>
          AdmissionPeriodCalculator(period, dateOffset)
        .exists:
          _.hasAdmissionPeriodStartForDay(localDate)

    def isPermitted(timestamp: Timestamp, dateOffset: FiniteDuration)(using ZoneId): Boolean =
      findTimeInterval(timestamp, dateOffset = dateOffset)
        .exists(_.contains(timestamp))

    /** Find current and next TimeIntervals until `until` for admission.
     * Shifts the interval when daylight saving time skips an hour. */
    def findTimeIntervals(
      from: Timestamp, until: Timestamp, dateOffset: FiniteDuration)(using ZoneId)
    : View[(Int, TimeInterval)] =
      val localFrom = from.toLocalDateTime
      findLocalIntervals(localFrom, dateOffset)
        .map: (periodIndex, localInterval) =>
          periodIndex -> localInterval.toTimeInterval
        .takeWhile(_._2.startsBefore(until)) // <-- delete FIXME
        .filterNot(_._2.endsBefore(from))

  /** Find a current or next TimeInterval for admission.
     * Shifts the interval when daylight saving time skips an hour. */
    def findTimeInterval(from: Timestamp, dateOffset: FiniteDuration)(using ZoneId)
    : Option[TimeInterval] =
      findLocalIntervals(from.toLocalDateTime, dateOffset)
        .map(_._2.toTimeInterval)
        .filterNot(_.endsBefore(from))
        .headOption

    /** Calculates end time with local time, yielding an hour more or less when dst shifts. */
    @TestOnly
    def findLocalInterval(from: LocalDateTime, dateOffset: FiniteDuration)(using ZoneId)
    : Option[LocalInterval] =
      findLocalIntervals(from, dateOffset)
        .map(_._2)
        .filterNot(_.endsBefore(from))
        .headOption

    /** Return View[(period index, LocalInterval)]). */
    private[time] def findLocalIntervals(
      local: LocalDateTime, dateOffset: FiniteDuration)(using ZoneId)
    : View[(Int, LocalInterval)] =
      View.fromIteratorProvider: () =>
        admissionTimeScheme.restrictedSchemes.view
          .flatMap: restrictedScheme =>
            restrictedScheme.periods.map(restrictedScheme.restriction -> _)
          .zipWithIndex
          .map:
            case ((restriction, period), i) =>
              AdmissionPeriodCalculator(period, dateOffset)
                .findLocalIntervals(local, restriction).map(i -> _)
          .mergeOrderedBy(_._2)
