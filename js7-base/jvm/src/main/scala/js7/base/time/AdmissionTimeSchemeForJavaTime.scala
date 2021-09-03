package js7.base.time

import java.time.{ZoneId, ZonedDateTime}
import js7.base.time.AdmissionPeriodForJavaTime._
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp

/** Code requiring java time (JVM) .*/
object AdmissionTimeSchemeForJavaTime
{
  implicit final class JavaAdmissionTimeSchemeJava(private val admissionTimeScheme: AdmissionTimeScheme)
  extends AnyVal
  {
    def isPermitted(timestamp: Timestamp, zone: ZoneId): Boolean =
      findTimeInterval(timestamp, zone).exists(_.contains(timestamp))

    def findTimeInterval(timestamp: Timestamp, zone: ZoneId): Option[TimeInterval] =
      findTimeInterval(timestamp.toZonedDateTime(zone))

    def findTimeInterval(zoned: ZonedDateTime): Option[TimeInterval] =
      findLocalInterval(zoned)
        .map(_.toTimeInterval(zoned.getZone))

    def findLocalInterval(zoned: ZonedDateTime): Option[LocalInterval] = {
      val local = zoned.toLocalDateTime
      admissionTimeScheme.periods.view
        .flatMap { admissionPeriod =>
          admissionPeriod.toInterval(local) ++
            // Look ahead in case of invalid local time due to start of daylight saving time
            // --> Could be done in findTimeInterval where LocalDate is checked
            admissionPeriod.toInterval(
              admissionPeriod.nextCalendarStart(local))
        }
        .filterNot(_.endsBefore(local))
        .minByOption(_.start)
    }
  }
}
