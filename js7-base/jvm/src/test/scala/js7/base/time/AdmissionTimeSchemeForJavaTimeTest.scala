package js7.base.time

import java.time.DayOfWeek.{FRIDAY, MONDAY, TUESDAY}
import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import js7.base.test.OurTestSuite
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts

final class AdmissionTimeSchemeForJavaTimeTest extends OurTestSuite:

  private val zone = ZoneId.of("Europe/Mariehamn")
  private val dateOffset = 3.h

  private val scheme = AdmissionTimeScheme(Seq(
    /*0*/DailyPeriod(LocalTime.of(9, 0), 2.h),
    /*1*/WeekdayPeriod(TUESDAY, LocalTime.of(18, 0), 2.h),
    /*2*/MonthlyWeekdayPeriod(2, MONDAY, LocalTime.of(19, 0), 2.h),
    /*3*/MonthlyLastWeekdayPeriod(-2, FRIDAY, LocalTime.of(20, 0), 1.h),
    /*4*/MonthlyLastDatePeriod(-1, LocalTime.of(21, 0), 3.h),
    /*5*/MonthlyDatePeriod(3, LocalTime.of(0, 0), 1.h),
    /*6*/SpecificDatePeriod(LocalDateTime.parse("2023-07-06T12:00"), 1.s)))

  "hasAdmissionPeriodStartForDay" in:
    val scheme = AdmissionTimeScheme(Seq(
      WeekdayPeriod(TUESDAY, LocalTime.of(18, 0), 2.h)))
    assert(!scheme.hasAdmissionPeriodStartForDay(LocalDate.parse("2023-07-03"), dateOffset))
    assert(scheme.hasAdmissionPeriodStartForDay(LocalDate.parse("2023-07-04"), dateOffset))
    assert(!scheme.hasAdmissionPeriodStartForDay(LocalDate.parse("2023-07-06"), dateOffset))

  "isPermitted" in:
    // 2023-04-06 is a tuesday
    assert(!scheme.isPermitted(ts"2023-07-04T00:00:00Z", zone, dateOffset))
    assert(!scheme.isPermitted(ts"2023-07-04T05:59:59Z", zone, dateOffset))
    assert(scheme.isPermitted(ts"2023-07-04T06:00:00Z", zone, dateOffset))

    assert(!scheme.isPermitted(ts"2023-07-04T14:59:59Z", zone, dateOffset))
    assert(scheme.isPermitted(ts"2023-07-04T15:00:00Z", zone, dateOffset))

  "findLocalInterval" in:
    assert(scheme.findLocalInterval(local("2023-07-04T00:00"), dateOffset) ==
      Some(LocalInterval(local("2023-07-04T09:00"), 2.h)))
    assert(scheme.findLocalInterval(local("2023-07-04T09:00"), dateOffset) ==
      Some(LocalInterval(local("2023-07-04T09:00"), 2.h)))
    assert(scheme.findLocalInterval(local("2023-07-04T10:59"), dateOffset) ==
      Some(LocalInterval(local("2023-07-04T09:00"), 2.h)))

    assert(scheme.findLocalInterval(local("2023-07-04T11:00"), dateOffset) ==
      Some(LocalInterval(local("2023-07-04T18:00"), 2.h)))

    def localIntervals(from: LocalDateTime, until: LocalDateTime): Seq[(Int, LocalInterval)] =
        scheme.findLocalIntervals(from, dateOffset)
          .takeWhile(_._2.startsBefore(until))
          .filterNot(_._2.endsBefore(from))
          .toSeq

    assert(localIntervals(local("2023-07-01T00:00"), until = local("2023-08-01T00:00")) == Seq(
      0 -> LocalInterval(local("2023-07-01T09:00"), 2.h), // saturday
      0 -> LocalInterval(local("2023-07-02T09:00"), 2.h), // sunday

      5 -> LocalInterval(local("2023-07-03T00:00"), 1.h), // monday
      0 -> LocalInterval(local("2023-07-03T09:00"), 2.h),
      0 -> LocalInterval(local("2023-07-04T09:00"), 2.h), // tuesday
      1 -> LocalInterval(local("2023-07-04T18:00"), 2.h),
      0 -> LocalInterval(local("2023-07-05T09:00"), 2.h), // wednesday
      0 -> LocalInterval(local("2023-07-06T09:00"), 2.h), // thursday
      6 -> LocalInterval(local("2023-07-06T12:00"), 1.s),
      0 -> LocalInterval(local("2023-07-07T09:00"), 2.h), // friday
      0 -> LocalInterval(local("2023-07-08T09:00"), 2.h), // saturday
      0 -> LocalInterval(local("2023-07-09T09:00"), 2.h), // sunday

      0 -> LocalInterval(local("2023-07-10T09:00"), 2.h), // monday
      2 -> LocalInterval(local("2023-07-10T19:00"), 2.h),
      0 -> LocalInterval(local("2023-07-11T09:00"), 2.h), // tuesday
      1 -> LocalInterval(local("2023-07-11T18:00"), 2.h),
      0 -> LocalInterval(local("2023-07-12T09:00"), 2.h), // wednesday
      0 -> LocalInterval(local("2023-07-13T09:00"), 2.h), // thursday
      0 -> LocalInterval(local("2023-07-14T09:00"), 2.h), // friday
      0 -> LocalInterval(local("2023-07-15T09:00"), 2.h), // saturday
      0 -> LocalInterval(local("2023-07-16T09:00"), 2.h), // sunday

      0 -> LocalInterval(local("2023-07-17T09:00"), 2.h), // monday
      0 -> LocalInterval(local("2023-07-18T09:00"), 2.h), // tuesday
      1 -> LocalInterval(local("2023-07-18T18:00"), 2.h),
      0 -> LocalInterval(local("2023-07-19T09:00"), 2.h), // wednesday
      0 -> LocalInterval(local("2023-07-20T09:00"), 2.h), // thursday
      0 -> LocalInterval(local("2023-07-21T09:00"), 2.h), // friday
      3 -> LocalInterval(local("2023-07-21T20:00"), 1.h),
      0 -> LocalInterval(local("2023-07-22T09:00"), 2.h), // saturday
      0 -> LocalInterval(local("2023-07-23T09:00"), 2.h), // sunday

      0 -> LocalInterval(local("2023-07-24T09:00"), 2.h), // monday
      0 -> LocalInterval(local("2023-07-25T09:00"), 2.h), // tuesday
      1 -> LocalInterval(local("2023-07-25T18:00"), 2.h),
      0 -> LocalInterval(local("2023-07-26T09:00"), 2.h), // wednesday
      0 -> LocalInterval(local("2023-07-27T09:00"), 2.h), // thursday
      0 -> LocalInterval(local("2023-07-28T09:00"), 2.h), // friday
      0 -> LocalInterval(local("2023-07-29T09:00"), 2.h), // saturday
      0 -> LocalInterval(local("2023-07-30T09:00"), 2.h), // sunday

      0 -> LocalInterval(local("2023-07-31T09:00"), 2.h), // monday
      4 -> LocalInterval(local("2023-07-31T21:00"), 3.h)))

  "findTimeInterval" in:
    assert(scheme.findTimeInterval(ts"2023-07-04T00:00:00Z", zone, dateOffset) ==
      Some(TimeInterval(ts"2023-07-04T06:00:00Z", 2.h)))
    assert(scheme.findTimeInterval(ts"2023-07-04T06:00:00Z", zone, dateOffset) ==
      Some(TimeInterval(ts"2023-07-04T06:00:00Z", 2.h)))
    assert(scheme.findTimeInterval(ts"2023-07-04T07:59:59Z", zone, dateOffset) ==
      Some(TimeInterval(ts"2023-07-04T06:00:00Z", 2.h)))

    assert(scheme.findTimeInterval(ts"2023-07-04T08:00:00Z", zone, dateOffset) ==
      Some(TimeInterval(ts"2023-07-04T15:00:00Z", 2.h)))

  "findTimeIntervals" in:
    val timeIntervals = scheme.findTimeIntervals(
      from = ts"2023-07-01T00:00:00Z",
      until = ts"2023-08-01T00:00:00Z",
      zone, dateOffset)
    assert(timeIntervals.toSeq == Seq(
       0 -> TimeInterval(ts"2023-07-01T06:00:00Z", 2.h), // saturday
       0 -> TimeInterval(ts"2023-07-02T06:00:00Z", 2.h), // sunday

       5 -> TimeInterval(ts"2023-07-02T21:00:00Z", 1.h), // monday
       0 -> TimeInterval(ts"2023-07-03T06:00:00Z", 2.h),
       0 -> TimeInterval(ts"2023-07-04T06:00:00Z", 2.h), // tuesday
       1 -> TimeInterval(ts"2023-07-04T15:00:00Z", 2.h),
       0 -> TimeInterval(ts"2023-07-05T06:00:00Z", 2.h), // wednesday
       0 -> TimeInterval(ts"2023-07-06T06:00:00Z", 2.h), // thursday
       6 -> TimeInterval(ts"2023-07-06T09:00:00Z", 1.s),
       0 -> TimeInterval(ts"2023-07-07T06:00:00Z", 2.h), // friday
       0 -> TimeInterval(ts"2023-07-08T06:00:00Z", 2.h), // saturday
       0 -> TimeInterval(ts"2023-07-09T06:00:00Z", 2.h), // sunday

       0 -> TimeInterval(ts"2023-07-10T06:00:00Z", 2.h), // monday
       2 -> TimeInterval(ts"2023-07-10T16:00:00Z", 2.h),
       0 -> TimeInterval(ts"2023-07-11T06:00:00Z", 2.h), // tuesday
       1 -> TimeInterval(ts"2023-07-11T15:00:00Z", 2.h),
       0 -> TimeInterval(ts"2023-07-12T06:00:00Z", 2.h), // wednesday
       0 -> TimeInterval(ts"2023-07-13T06:00:00Z", 2.h), // thursday
       0 -> TimeInterval(Timestamp("2023-07-14T06:00:00Z"), 2.h), // friday
       0 -> TimeInterval(Timestamp("2023-07-15T06:00:00Z"), 2.h), // saturday
       0 -> TimeInterval(Timestamp("2023-07-16T06:00:00Z"), 2.h), // sunday

       0 -> TimeInterval(Timestamp("2023-07-17T06:00:00Z"), 2.h), // monday
       0 -> TimeInterval(Timestamp("2023-07-18T06:00:00Z"), 2.h), // tuesday
       1 -> TimeInterval(Timestamp("2023-07-18T15:00:00Z"), 2.h),
       0 -> TimeInterval(Timestamp("2023-07-19T06:00:00Z"), 2.h), // wednesday
       0 -> TimeInterval(Timestamp("2023-07-20T06:00:00Z"), 2.h), // thursday
       0 -> TimeInterval(Timestamp("2023-07-21T06:00:00Z"), 2.h), // friday
       3 -> TimeInterval(Timestamp("2023-07-21T17:00:00Z"), 1.h),
       0 -> TimeInterval(Timestamp("2023-07-22T06:00:00Z"), 2.h), // saturday
       0 -> TimeInterval(Timestamp("2023-07-23T06:00:00Z"), 2.h), // sunday

       0 -> TimeInterval(Timestamp("2023-07-24T06:00:00Z"), 2.h), // monday
       0 -> TimeInterval(Timestamp("2023-07-25T06:00:00Z"), 2.h), // tuesday
       1 -> TimeInterval(Timestamp("2023-07-25T15:00:00Z"), 2.h),
       0 -> TimeInterval(Timestamp("2023-07-26T06:00:00Z"), 2.h), // wednesday
       0 -> TimeInterval(Timestamp("2023-07-27T06:00:00Z"), 2.h), // thursday
       0 -> TimeInterval(Timestamp("2023-07-28T06:00:00Z"), 2.h), // friday
       0 -> TimeInterval(Timestamp("2023-07-29T06:00:00Z"), 2.h), // saturday
       0 -> TimeInterval(Timestamp("2023-07-30T06:00:00Z"), 2.h), // sunday

       0 -> TimeInterval(Timestamp("2023-07-31T06:00:00Z"), 2.h), // monday
       4 -> TimeInterval(Timestamp("2023-07-31T18:00:00Z"), 3.h)))

  private def local(string: String) = LocalDateTime.parse(string)
