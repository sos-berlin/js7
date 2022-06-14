package js7.base.time

import java.time.{LocalDate, LocalDateTime, ZoneId}
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.concurrent.duration.FiniteDuration

/** Code requiring java time (JVM) .*/
object AdmissionTimeSchemeForJavaTime
{
  implicit final class RichAdmissionTimeScheme(private val admissionTimeScheme: AdmissionTimeScheme)
  extends AnyVal
  {
    def hasPeriodForDay(localDate: LocalDate, dateOffset: FiniteDuration): Boolean =
      admissionTimeScheme.periods
        .view
        .map(AdmissionPeriodCalculator(_, dateOffset))
        .exists(_.hasPeriodForDay(localDate))

    def isPermitted(timestamp: Timestamp, zone: ZoneId, dateOffset: FiniteDuration): Boolean =
      findTimeInterval(timestamp, zone, dateOffset)
        .exists(_.contains(timestamp))

    /** Find a current or next TimeInterval for admission.
     * Shifts the interval when daylight saving time skips an hour. */
    def findTimeInterval(timestamp: Timestamp, zone: ZoneId, dateOffset: FiniteDuration)
    : Option[TimeInterval] =
      findLocalIntervals(timestamp.toLocalDateTime(zone), dateOffset)
        .map(_.toTimeInterval(zone))
        .filterNot(_.endsBefore(timestamp))
        .minByOption(_.start)

    /** Calculates end time with local time, yielding a hour more or less when dst shifts. */
    @TestOnly
    def findLocalInterval(local: LocalDateTime, dateOffset: FiniteDuration): Option[LocalInterval] =
      findLocalIntervals(local, dateOffset)
        .filterNot(_.endsBefore(local))
        .minOption

    private def findLocalIntervals(local: LocalDateTime, dateOffset: FiniteDuration)
    : View[LocalInterval] =
      admissionTimeScheme.periods
        .view
        .map(AdmissionPeriodCalculator(_, dateOffset))
        .flatMap { executor =>
          executor.toLocalInterval(local) ++
            // Look ahead in case of invalid local time due to start of daylight saving time
            // --> Could be done in findTimeInterval where LocalDate is checked
            executor.nextCalendarPeriodStart(local)
              .flatMap(executor.toLocalInterval)
        }
  }
}
