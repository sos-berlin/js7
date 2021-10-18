package js7.base.time

import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import scala.concurrent.duration.FiniteDuration

/** Code requiring java time (JVM) .*/
object AdmissionTimeSchemeForJavaTime
{
  implicit final class JavaAdmissionTimeSchemeJava(private val admissionTimeScheme: AdmissionTimeScheme)
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

    def findTimeInterval(timestamp: Timestamp, zone: ZoneId, dateOffset: FiniteDuration)
    : Option[TimeInterval] =
      findTimeInterval(timestamp.toZonedDateTime(zone), dateOffset)

    def findTimeInterval(zoned: ZonedDateTime, dateOffset: FiniteDuration): Option[TimeInterval] =
      findLocalInterval(zoned.toLocalDateTime, dateOffset)
        .map(_.toTimeInterval(zoned.getZone))

    def findLocalInterval(local: LocalDateTime, dateOffset: FiniteDuration): Option[LocalInterval] =
      admissionTimeScheme.periods
        .view
        .map(AdmissionPeriodCalculator(_, dateOffset))
        .flatMap { executor =>
          executor.toLocalInterval(local) ++
            // Look ahead in case of invalid local time due to start of daylight saving time
            // --> Could be done in findTimeInterval where LocalDate is checked
            executor.toLocalInterval(
              executor.nextCalendarStart(local))
        }
        .filterNot(_.endsBefore(local))
        .minByOption(_.start)
  }
}
