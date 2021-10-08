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
        DailyPeriod(LocalTime.of(12, 0), 1.h),
        AlwaysPeriod)),
      json"""{
        "periods": [
          {
            "TYPE": "WeekdayPeriod",
            "secondOfWeek": 115200,
            "duration": 7200
          }, {
            "TYPE": "DailyPeriod",
            "secondOfDay": 43200,
            "duration": 3600
          }, {
            "TYPE": "AlwaysPeriod"
          }
        ]
      }""")

    testJson(
      AdmissionTimeScheme(Seq(AlwaysPeriod)),
      json"""{
        "periods": [
          {
            "TYPE": "AlwaysPeriod"
          }
        ]
      }""")
  }

  "findLocalInterval" - {
    "Empty" in {
      implicit val admissionTimeScheme = AdmissionTimeScheme(Nil)
      assert(findLocalInterval("2021-08-23T06:00:00") == None)
    }

    "WeekdayPeriod" - {
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

      "WeekdayPeriod overlap from previous week" in {
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

    "WeekdayPeriod and DailyPeriod" in {
      implicit val admissionTimeScheme = AdmissionTimeScheme(Seq(
        WeekdayPeriod(TUESDAY, LocalTime.of(8, 0), 2.h),
        WeekdayPeriod(SUNDAY, LocalTime.of(22, 0), 1.h),
        DailyPeriod(LocalTime.of(12, 0), 1.h)))

      assert(findLocalInterval("2021-08-23T06:00") ==
        Some(localInterval("2021-08-23T12:00", 1.h)))

      assert(findLocalInterval("2021-08-24T06:00") ==
        Some(localInterval("2021-08-24T08:00", 2.h)))
      assert(findLocalInterval("2021-08-24T09:59") ==
        Some(localInterval("2021-08-24T08:00", 2.h)))
      assert(findLocalInterval("2021-08-24T10:10") ==

        Some(localInterval("2021-08-24T12:00", 1.h)))
      assert(findLocalInterval("2021-08-24T12:00") ==
        Some(localInterval("2021-08-24T12:00", 1.h)))
      assert(findLocalInterval("2021-08-24T12:59") ==
        Some(localInterval("2021-08-24T12:00", 1.h)))

      assert(findLocalInterval("2021-08-24T13:00") ==
        Some(localInterval("2021-08-25T12:00", 1.h)))

      assert(findLocalInterval("2021-08-29T13:00") ==
        Some(localInterval("2021-08-29T22:00", 1.h)))
    }
  }
}

object AdmissionTimeSchemeTest
{
  def findLocalInterval(dateTimeString: String)(implicit admissionTimeScheme: AdmissionTimeScheme)
  : Option[LocalInterval] =
    admissionTimeScheme.findLocalInterval(
      LocalDateTime.parse(dateTimeString))

  def localInterval(dateTimeString: String, duration: FiniteDuration): LocalInterval =
    LocalInterval(LocalDateTime.parse(dateTimeString), duration)
}
