package js7.base.time

import java.time.DayOfWeek.{SUNDAY, TUESDAY}
import java.time.ZoneOffset.UTC
import java.time.{LocalDateTime, LocalTime, ZonedDateTime}
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.time.AdmissionTimeSchemeForJavaTime._
import js7.base.time.AdmissionTimeSchemeTest._
import js7.base.time.ScalaTime._
import js7.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.FiniteDuration

final class AdmissionTimeSchemeTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      AdmissionTimeScheme(Seq(
        WeekdayPeriod(TUESDAY, LocalTime.of(8, 0), 2.h),
        WeekdayPeriod(SUNDAY, LocalTime.of(22, 0), 1.h))),
      json"""{
        "periods": [
          {
            "TYPE": "WeekdayPeriod",
            "secondOfWeek": 115200,
            "duration": 7200
          }, {
            "TYPE": "WeekdayPeriod",
            "secondOfWeek": 597600,
            "duration": 3600
          }
        ]
      }""")
  }

  "findLocalInterval" - {
    "Empty" in {
      implicit val admissionTimeScheme = AdmissionTimeScheme(Nil)
      assert(findLocalInterval("2021-08-23T06:00:00") == None)
    }

    "Simple" in {
      implicit val admissionTimeScheme = AdmissionTimeScheme(Seq(
        WeekdayPeriod(TUESDAY, LocalTime.of(8, 0), 2.h),
        WeekdayPeriod(SUNDAY, LocalTime.of(22, 0), 1.h)))

      assert(findLocalInterval("2021-08-23T06:00") ==
        Some(localInterval("2021-08-24T08:00", 2.h)))
      assert(findLocalInterval("2021-08-24T07:59") ==
        Some(localInterval("2021-08-24T08:00", 2.h)))
      assert(findLocalInterval("2021-08-24T08:00") ==
        Some(localInterval("2021-08-24T08:00", 2.h)))
      assert(findLocalInterval("2021-08-24T09:59") ==
        Some(localInterval("2021-08-24T08:00", 2.h)))
      assert(findLocalInterval("2021-08-24T10:00") ==
        Some(localInterval("2021-08-29T22:00", 1.h)))
    }

    "Overlap from previous week" in {
      implicit val admissionTimeScheme = AdmissionTimeScheme(Seq(
        WeekdayPeriod(TUESDAY, LocalTime.of(8, 0), 2.h),
        WeekdayPeriod(SUNDAY, LocalTime.of(22, 0), 3.h)))

      assert(findLocalInterval("2021-08-30T00:00") ==
        Some(localInterval("2021-08-29T22:00", 3.h)))
      assert(findLocalInterval("2021-08-30T00:59") ==
        Some(localInterval("2021-08-29T22:00", 3.h)))
      assert(findLocalInterval("2021-08-30T02:00") ==
        Some(localInterval("2021-08-31T08:00", 2.h)))
    }
  }
}

object AdmissionTimeSchemeTest
{
  def findLocalInterval(dateTimeString: String)(implicit admissionTimeScheme: AdmissionTimeScheme)
  : Option[LocalInterval] =
    admissionTimeScheme.findLocalInterval(
      ZonedDateTime.of(LocalDateTime.parse(dateTimeString), UTC))

  def localInterval(dateTimeString: String, duration: FiniteDuration): LocalInterval =
    LocalInterval(LocalDateTime.parse(dateTimeString), duration)
}
