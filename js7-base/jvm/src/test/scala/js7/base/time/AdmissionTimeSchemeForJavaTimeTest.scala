package js7.base.time

import java.time.DayOfWeek.{FRIDAY, MONDAY, TUESDAY}
import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import js7.base.test.OurTestSuite
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts

final class AdmissionTimeSchemeForJavaTimeTest extends OurTestSuite:

  private val zone = ZoneId.of("Europe/Mariehamn")
  private val dateOffset = 5.h

  private val scheme = AdmissionTimeScheme(Seq(
    /*0*/DailyPeriod(LocalTime.of(2, 0), 4.h), // Due to dateOffset = 5h, period starts the day before
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
    assert(!scheme.isPermitted(ts"2023-07-03T22:59:59Z", zone, dateOffset))
    assert(scheme.isPermitted(ts"2023-07-03T23:00:00Z", zone, dateOffset)) // DailyPeriod(LocalTime.of(2, 0), 2.h),
    assert(scheme.isPermitted(ts"2023-07-04T02:59:59Z", zone, dateOffset))
    assert(!scheme.isPermitted(ts"2023-07-04T03:00:00Z", zone, dateOffset))

    assert(!scheme.isPermitted(ts"2023-07-04T14:59:59Z", zone, dateOffset))
    assert(scheme.isPermitted(ts"2023-07-04T15:00:00Z", zone, dateOffset))

  "findLocalInterval" in:
    assert(scheme.findLocalInterval(local("2023-07-04T00:00"), dateOffset) ==
      Some(LocalInterval(local("2023-07-04T02:00"), 4.h)))
    assert(scheme.findLocalInterval(local("2023-07-04T02:00"), dateOffset) ==
      Some(LocalInterval(local("2023-07-04T02:00"), 4.h)))
    assert(scheme.findLocalInterval(local("2023-07-04T03:59"), dateOffset) ==
      Some(LocalInterval(local("2023-07-04T02:00"), 4.h)))

    assert(scheme.findLocalInterval(local("2023-07-04T06:00"), dateOffset) ==
      Some(LocalInterval(local("2023-07-04T18:00"), 2.h)))

    def localIntervals(from: LocalDateTime, until: LocalDateTime): Seq[(Int, LocalInterval)] =
      scheme.findLocalIntervals(from, dateOffset)
        .takeWhile(_._2.startsBefore(until))
        .filterNot(_._2.endsBefore(from))
        .toSeq

    assert(localIntervals(local("2023-07-01T00:00"), until = local("2023-08-01T00:00")) == Seq(
      0 -> LocalInterval(local("2023-07-01T02:00"), 4.h), // saturday
      0 -> LocalInterval(local("2023-07-02T02:00"), 4.h), // sunday

      5 -> LocalInterval(local("2023-07-03T00:00"), 1.h), // monday
      0 -> LocalInterval(local("2023-07-03T02:00"), 4.h),
      0 -> LocalInterval(local("2023-07-04T02:00"), 4.h), // tuesday
      1 -> LocalInterval(local("2023-07-04T18:00"), 2.h),
      0 -> LocalInterval(local("2023-07-05T02:00"), 4.h), // wednesday
      0 -> LocalInterval(local("2023-07-06T02:00"), 4.h), // thursday
      6 -> LocalInterval(local("2023-07-06T12:00"), 1.s),
      0 -> LocalInterval(local("2023-07-07T02:00"), 4.h), // friday
      0 -> LocalInterval(local("2023-07-08T02:00"), 4.h), // saturday
      0 -> LocalInterval(local("2023-07-09T02:00"), 4.h), // sunday

      0 -> LocalInterval(local("2023-07-10T02:00"), 4.h), // monday
      2 -> LocalInterval(local("2023-07-10T19:00"), 2.h),
      0 -> LocalInterval(local("2023-07-11T02:00"), 4.h), // tuesday
      1 -> LocalInterval(local("2023-07-11T18:00"), 2.h),
      0 -> LocalInterval(local("2023-07-12T02:00"), 4.h), // wednesday
      0 -> LocalInterval(local("2023-07-13T02:00"), 4.h), // thursday
      0 -> LocalInterval(local("2023-07-14T02:00"), 4.h), // friday
      0 -> LocalInterval(local("2023-07-15T02:00"), 4.h), // saturday
      0 -> LocalInterval(local("2023-07-16T02:00"), 4.h), // sunday

      0 -> LocalInterval(local("2023-07-17T02:00"), 4.h), // monday
      0 -> LocalInterval(local("2023-07-18T02:00"), 4.h), // tuesday
      1 -> LocalInterval(local("2023-07-18T18:00"), 2.h),
      0 -> LocalInterval(local("2023-07-19T02:00"), 4.h), // wednesday
      0 -> LocalInterval(local("2023-07-20T02:00"), 4.h), // thursday
      0 -> LocalInterval(local("2023-07-21T02:00"), 4.h), // friday
      3 -> LocalInterval(local("2023-07-21T20:00"), 1.h),
      0 -> LocalInterval(local("2023-07-22T02:00"), 4.h), // saturday
      0 -> LocalInterval(local("2023-07-23T02:00"), 4.h), // sunday

      0 -> LocalInterval(local("2023-07-24T02:00"), 4.h), // monday
      0 -> LocalInterval(local("2023-07-25T02:00"), 4.h), // tuesday
      1 -> LocalInterval(local("2023-07-25T18:00"), 2.h),
      0 -> LocalInterval(local("2023-07-26T02:00"), 4.h), // wednesday
      0 -> LocalInterval(local("2023-07-27T02:00"), 4.h), // thursday
      0 -> LocalInterval(local("2023-07-28T02:00"), 4.h), // friday
      0 -> LocalInterval(local("2023-07-29T02:00"), 4.h), // saturday
      0 -> LocalInterval(local("2023-07-30T02:00"), 4.h), // sunday

      0 -> LocalInterval(local("2023-07-31T02:00"), 4.h), // monday
      4 -> LocalInterval(local("2023-07-31T21:00"), 3.h)))

  "findTimeInterval" in:
    assert(scheme.findTimeInterval(ts"2023-07-03T23:00:00Z", zone, dateOffset) ==
      Some(TimeInterval(ts"2023-07-03T23:00:00Z", 4.h))) // DailyPeriod(LocalTime.of(2, 0), 2.h),
    assert(scheme.findTimeInterval(ts"2023-07-04T23:00:00Z", zone, dateOffset) ==
      Some(TimeInterval(ts"2023-07-04T23:00:00Z", 4.h)))
    assert(scheme.findTimeInterval(ts"2023-07-04T22:59:59Z", zone, dateOffset) ==
      Some(TimeInterval(ts"2023-07-04T23:00:00Z", 4.h)))

    assert(scheme.findTimeInterval(ts"2023-07-04T08:00:00Z", zone, dateOffset) ==
      Some(TimeInterval(ts"2023-07-04T15:00:00Z", 2.h)))

  "findTimeIntervals" in:
    val timeIntervals = scheme.findTimeIntervals(
      from = ts"2023-07-01T00:00:00Z",
      until = ts"2023-08-01T00:00:00Z",
      zone, dateOffset)
    assert(timeIntervals.toSeq == Seq(
      0 -> TimeInterval(ts"2023-06-30T23:00:00Z", 4.h), // saturday (dateOffset = 5h!)
      0 -> TimeInterval(ts"2023-07-01T23:00:00Z", 4.h), // sunday

      5 -> TimeInterval(ts"2023-07-02T21:00:00Z", 1.h), // monday
      0 -> TimeInterval(ts"2023-07-02T23:00:00Z", 4.h),
      0 -> TimeInterval(ts"2023-07-03T23:00:00Z", 4.h), // tuesday
      1 -> TimeInterval(ts"2023-07-04T15:00:00Z", 2.h),
      0 -> TimeInterval(ts"2023-07-04T23:00:00Z", 4.h), // wednesday
      0 -> TimeInterval(ts"2023-07-05T23:00:00Z", 4.h), // thursday
      6 -> TimeInterval(ts"2023-07-06T09:00:00Z", 1.s),
      0 -> TimeInterval(ts"2023-07-06T23:00:00Z", 4.h), // friday
      0 -> TimeInterval(ts"2023-07-07T23:00:00Z", 4.h), // saturday
      0 -> TimeInterval(ts"2023-07-08T23:00:00Z", 4.h), // sunday

      0 -> TimeInterval(ts"2023-07-09T23:00:00Z", 4.h), // monday
      2 -> TimeInterval(ts"2023-07-10T16:00:00Z", 2.h),
      0 -> TimeInterval(ts"2023-07-10T23:00:00Z", 4.h), // tuesday
      1 -> TimeInterval(ts"2023-07-11T15:00:00Z", 2.h),
      0 -> TimeInterval(ts"2023-07-11T23:00:00Z", 4.h), // wednesday
      0 -> TimeInterval(ts"2023-07-12T23:00:00Z", 4.h), // thursday
      0 -> TimeInterval(ts"2023-07-13T23:00:00Z", 4.h), // friday
      0 -> TimeInterval(ts"2023-07-14T23:00:00Z", 4.h), // saturday
      0 -> TimeInterval(ts"2023-07-15T23:00:00Z", 4.h), // sunday

      0 -> TimeInterval(ts"2023-07-16T23:00:00Z", 4.h), // monday
      0 -> TimeInterval(ts"2023-07-17T23:00:00Z", 4.h), // tuesday
      1 -> TimeInterval(ts"2023-07-18T15:00:00Z", 2.h),
      0 -> TimeInterval(ts"2023-07-18T23:00:00Z", 4.h), // wednesday
      0 -> TimeInterval(ts"2023-07-19T23:00:00Z", 4.h), // thursday
      0 -> TimeInterval(ts"2023-07-20T23:00:00Z", 4.h), // friday
      3 -> TimeInterval(ts"2023-07-21T17:00:00Z", 1.h),
      0 -> TimeInterval(ts"2023-07-21T23:00:00Z", 4.h), // saturday
      0 -> TimeInterval(ts"2023-07-22T23:00:00Z", 4.h), // sunday

      0 -> TimeInterval(ts"2023-07-23T23:00:00Z", 4.h), // monday
      0 -> TimeInterval(ts"2023-07-24T23:00:00Z", 4.h), // tuesday
      1 -> TimeInterval(ts"2023-07-25T15:00:00Z", 2.h),
      0 -> TimeInterval(ts"2023-07-25T23:00:00Z", 4.h), // wednesday
      0 -> TimeInterval(ts"2023-07-26T23:00:00Z", 4.h), // thursday
      0 -> TimeInterval(ts"2023-07-27T23:00:00Z", 4.h), // friday
      0 -> TimeInterval(ts"2023-07-28T23:00:00Z", 4.h), // saturday
      0 -> TimeInterval(ts"2023-07-29T23:00:00Z", 4.h), // sunday

      0 -> TimeInterval(ts"2023-07-30T23:00:00Z", 4.h), // monday
      4 -> TimeInterval(ts"2023-07-31T18:00:00Z", 3.h),
      0 -> TimeInterval(ts"2023-07-31T23:00:00Z", 4.h)))

  private def local(string: String) = LocalDateTime.parse(string)
