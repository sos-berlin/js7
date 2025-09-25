package js7.base.time

import java.time.{LocalDate, LocalDateTime, ZoneId}
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime.RichFiniteDurationCompanion
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.concurrent.duration.FiniteDuration
import scala.util.boundary

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
      findTimeInterval(timestamp, limit = FiniteDuration.Epsilon, dateOffset = dateOffset)
        .exists(_.contains(timestamp))

    /** Find current and next TimeIntervals until `until` for admission.
     * Shifts the interval when daylight saving time skips an hour. */
    def findTimeIntervals(
      from: Timestamp, until: Timestamp, dateOffset: FiniteDuration)(using ZoneId)
    : View[(Int, TimeInterval)] =
      findLocalIntervals(from.toLocalDateTime, until.toLocalDateTime, dateOffset)
        .map: (periodIndex, localInterval) =>
          periodIndex -> localInterval.toTimeInterval

  /** Find a current or next TimeInterval for admission.
     * Shifts the interval when daylight saving time skips an hour. */
    def findTimeInterval(from: Timestamp, limit: FiniteDuration, dateOffset: FiniteDuration)
      (using ZoneId)
    : Option[TimeInterval] =
      findLocalIntervals(from.toLocalDateTime, until = (from + limit).toLocalDateTime, dateOffset)
        .map(_._2.toTimeInterval)
        .filterNot(_.endsBefore(from))
        .headOption

    /** Like findTimeInterval, but combines seamless following or overlapping TimeIntervals. */
    def findLongTimeInterval(
      from: Timestamp, limit: FiniteDuration, dateOffset: FiniteDuration)(using ZoneId)
    : Option[TimeInterval] =
      val timeIntervals = findTimeIntervals(from, from + limit, dateOffset).map(_._2)
      val it = timeIntervals.iterator
      it.hasNext.thenSome:
        var result = it.next()
        boundary:
          while it.hasNext do
            val b = it.next()
            result.tryCombine(b) match
              case Some(combined) => result = combined
              case None => boundary.break()
        result

    /** Calculates end time with local time, yielding an hour more or less when dst shifts. */
    @TestOnly
    def findLocalInterval(from: LocalDateTime, until: LocalDateTime, dateOffset: FiniteDuration)
      (using ZoneId)
    : Option[LocalInterval] =
      findLocalIntervals(from, until, dateOffset)
        .headOption.map(_._2)

    /** Return View[(period index, LocalInterval)]). */
    private[time] def findLocalIntervals(
      from: LocalDateTime, until: LocalDateTime, dateOffset: FiniteDuration)(using ZoneId)
    : View[(Int, LocalInterval)] =
      View.fromIteratorProvider: () =>
        admissionTimeScheme.restrictedSchemes.view
          .flatMap: restrictedScheme =>
            restrictedScheme.periods.map(restrictedScheme.restriction -> _)
          .zipWithIndex
          .map:
            case ((restriction, period), i) =>
              AdmissionPeriodCalculator(period, dateOffset)
                .findLocalIntervals(from, until, restriction).map(i -> _)
          .mergeOrderedBy(_._2)
