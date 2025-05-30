package js7.data.execution.workflow.instructions

import java.time.DayOfWeek.{FRIDAY, MONDAY, SATURDAY, SUNDAY, THURSDAY, TUESDAY, WEDNESDAY}
import java.time.LocalTime.MIDNIGHT
import java.time.{DayOfWeek, LocalDate, LocalDateTime, LocalTime, ZoneId, ZonedDateTime}
import java.util.Locale
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimeConverters.RichZonedDateTime
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, DailyPeriod, TimeInterval, Timestamp, WeekdayPeriod}
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.execution.workflow.instructions.ScheduleTester.*
import js7.data.order.CycleState
import js7.data.workflow.instructions.Schedule
import js7.data.workflow.instructions.Schedule.{Continuous, Periodic, Scheme, Ticking}
import org.scalactic.source
import scala.collection.View
import scala.concurrent.duration.*
import scala.jdk.DurationConverters.ScalaDurationOps

trait ScheduleTester extends OurTestSuite:

  protected final def addStandardScheduleTests
    (testDay: (TimeInterval, FiniteDuration, ZoneId, Seq[(Timestamp, CycleState)], Boolean) => Unit)
    (implicit pos: source.Position)
  : Unit =
    for ((onlyOnePeriod, days, timeOfDay) <- View(
      (false, standardSetting, MIDNIGHT.plus(dateOffset.toJava)),
      (true, onlyOnePeriodSetting, onlyOnePeriodTimeOfDay)))
      s"onlyOnePeriod=$onlyOnePeriod" - {
        for day <- days do day.testName in:
          assert(day.dayOfWeek == day.date.getDayOfWeek, "Weekday does not match start date")
          val startOfDay = LocalDateTime.of(day.date, MIDNIGHT)
          val localStart = startOfDay.plusSeconds(timeOfDay.toSecondOfDay)
          val localEnd = startOfDay.plus(dateOffset.toJava).plusDays(1)
          val start = ZonedDateTime.of(localStart, zone).toTimestamp
          val end = ZonedDateTime.of(localEnd, zone).toTimestamp
          testDay(
            start -> end,
            day.cycleDuration,
            zone,
            day.expectedCycles.map { case (now, cs) => now -> cs.toCycleState(end) },
            onlyOnePeriod)
      }


object ScheduleTester:

  val dateOffset = 6.h // Business day starts at 6:00 (i.e., switching from Monday to Tuesday)
  private implicit val zone: ZoneId = ZoneId.of("Europe/Mariehamn")

  val schedule: Schedule =
    import LocalTime.parse as localTime
    Schedule(Seq(
      Scheme(
        AdmissionTimeScheme(Seq(
          DailyPeriod(localTime("09:00"), 1.h + 20.minutes),
          DailyPeriod(localTime("12:00"), 16.minutes))),
        Periodic(
          period = 1.h,
          offsets = Seq(10.minute, 15.minute, 20.minute))),

      Scheme(
        AdmissionTimeScheme(Seq(
          WeekdayPeriod(TUESDAY, localTime("02:00"), 1.h), // Business day Monday night
          WeekdayPeriod(SATURDAY, localTime("04:00"), 1.h))), // Business day Friday night
        Ticking(20.minutes)),

      Scheme(
        AdmissionTimeScheme(Seq(
          WeekdayPeriod(SUNDAY, localTime("20:00"), 30.minutes))),
        Continuous(
          limit = Some(3),
          pause = 1.minute)),

      Scheme(
        AdmissionTimeScheme(Seq(
          WeekdayPeriod(SUNDAY, localTime("18:00"), 30.minutes))),
        Continuous(
          pause = 5.minutes))))

  private val standardSetting = Seq(
    Day(MONDAY,
      date = LocalDate.parse("2021-10-04"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-04T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-04T09:10")),
        local("2021-10-04T09:10") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-04T09:15")),
        local("2021-10-04T09:15") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-04T09:20")),
        local("2021-10-04T09:20") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-04T10:10")),
        local("2021-10-04T10:10") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-04T10:15")),

        local("2021-10-04T10:15") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-04T12:10")),
        local("2021-10-04T12:10") -> CS(scheme = 0, period = 1, i = 2, next = local("2021-10-04T12:15")),

        // Ticking (business day ends 6:00)
        local("2021-10-04T12:15") -> CS(scheme = 1, period = 0, i = 1, next = local("2021-10-05T02:00")),
        local("2021-10-05T02:00") -> CS(scheme = 1, period = 0, i = 2, next = local("2021-10-05T02:20")),
        local("2021-10-05T02:20") -> CS(scheme = 1, period = 0, i = 3, next = local("2021-10-05T02:40")))),

    Day(TUESDAY,
      date = LocalDate.parse("2021-10-05"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-05T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-05T09:10")),
        local("2021-10-05T09:10") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-05T09:15")),
        local("2021-10-05T09:15") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-05T09:20")),
        local("2021-10-05T09:20") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-05T10:10")),
        local("2021-10-05T10:10") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-05T10:15")),

        local("2021-10-05T10:15") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-05T12:10")),
        local("2021-10-05T12:10") -> CS(scheme = 0, period = 1, i = 2, next = local("2021-10-05T12:15")))),

    Day(WEDNESDAY,
      date = LocalDate.parse("2021-10-06"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-06T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-06T09:10")),
        local("2021-10-06T09:10") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-06T09:15")),
        local("2021-10-06T09:15") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-06T09:20")),
        local("2021-10-06T09:20") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-06T10:10")),
        local("2021-10-06T10:10") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-06T10:15")),

        local("2021-10-06T10:15") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-06T12:10")),
        local("2021-10-06T12:10") -> CS(scheme = 0, period = 1, i = 2, next = local("2021-10-06T12:15")),
      )),

    Day(THURSDAY,
      date = LocalDate.parse("2021-10-07"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-07T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-07T09:10")),
        local("2021-10-07T09:10") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-07T09:15")),
        local("2021-10-07T09:15") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-07T09:20")),
        local("2021-10-07T09:20") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-07T10:10")),
        local("2021-10-07T10:10") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-07T10:15")),

        local("2021-10-07T10:15") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-07T12:10")),
        local("2021-10-07T12:10") -> CS(scheme = 0, period = 1, i = 2, next = local("2021-10-07T12:15")))),

    Day(FRIDAY,
      date = LocalDate.parse("2021-10-08"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-08T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-08T09:10")),
        local("2021-10-08T09:10") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-08T09:15")),
        local("2021-10-08T09:15") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-08T09:20")),
        local("2021-10-08T09:20") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-08T10:10")),
        local("2021-10-08T10:10") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-08T10:15")),

        local("2021-10-08T10:15") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-08T12:10")),
        local("2021-10-08T12:10") -> CS(scheme = 0, period = 1, i = 2, next = local("2021-10-08T12:15")),

        // Ticking (still business calendar friday)
        local("2021-10-08T12:15") -> CS(scheme = 1, period = 1, i = 1, next = local("2021-10-09T04:00")),
        local("2021-10-09T04:00") -> CS(scheme = 1, period = 1, i = 2, next = local("2021-10-09T04:20")),
        local("2021-10-09T04:20") -> CS(scheme = 1, period = 1, i = 3, next = local("2021-10-09T04:40")))),

    Day(SATURDAY,
      date = LocalDate.parse("2021-10-09"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-09T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-09T09:10")),
        local("2021-10-09T09:10") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-09T09:15")),
        local("2021-10-09T09:15") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-09T09:20")),
        local("2021-10-09T09:20") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-09T10:10")),
        local("2021-10-09T10:10") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-09T10:15")),

        local("2021-10-09T10:15") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-09T12:10")),
        local("2021-10-09T12:10") -> CS(scheme = 0, period = 1, i = 2, next = local("2021-10-09T12:15")),
      )),

    Day(SUNDAY, title = "Continuous, with zero execution time",
      date = LocalDate.parse("2021-10-10"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-10T09:10")),
        local("2021-10-10T09:10") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-10T09:15")),
        local("2021-10-10T09:15") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:20") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-10T10:10")),
        local("2021-10-10T10:10") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-10T10:15")),

        local("2021-10-10T10:15") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-10T12:10")),
        local("2021-10-10T12:10") -> CS(scheme = 0, period = 1, i = 2, next = local("2021-10-10T12:15")),

        // Continuous
        local("2021-10-10T12:15") -> CS(scheme = 3, period = 0, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:00") -> CS(scheme = 3, period = 0, i = 2, next = local("2021-10-10T18:05")),
        local("2021-10-10T18:05") -> CS(scheme = 3, period = 0, i = 3, next = local("2021-10-10T18:10")),
        local("2021-10-10T18:10") -> CS(scheme = 3, period = 0, i = 4, next = local("2021-10-10T18:15")),
        local("2021-10-10T18:15") -> CS(scheme = 3, period = 0, i = 5, next = local("2021-10-10T18:20")),
        local("2021-10-10T18:20") -> CS(scheme = 3, period = 0, i = 6, next = local("2021-10-10T18:25")),

        // Continuous
        local("2021-10-10T18:25") -> CS(scheme = 2, period = 0, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:00") -> CS(scheme = 2, period = 0, i = 2, next = local("2021-10-10T20:01")),
        local("2021-10-10T20:01") -> CS(scheme = 2, period = 0, i = 3, next = local("2021-10-10T20:02")))),

    Day(SUNDAY, title = "Continuous, with execution time shorter than cycle interval",
      date = LocalDate.parse("2021-10-10"),
      cycleDuration = 3.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-10T09:10")),
        local("2021-10-10T09:13") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-10T09:15")),
        local("2021-10-10T09:18") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:23") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-10T10:10")),
        local("2021-10-10T10:13") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-10T10:15")),

        local("2021-10-10T10:18") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-10T12:10")),
        local("2021-10-10T12:13") -> CS(scheme = 0, period = 1, i = 2, next = local("2021-10-10T12:15")),

        // Continuous
        local("2021-10-10T12:18") -> CS(scheme = 3, period = 0, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:03") -> CS(scheme = 3, period = 0, i = 2, next = local("2021-10-10T18:08")),
        local("2021-10-10T18:11") -> CS(scheme = 3, period = 0, i = 3, next = local("2021-10-10T18:16")),
        local("2021-10-10T18:19") -> CS(scheme = 3, period = 0, i = 4, next = local("2021-10-10T18:24")),

        // Continuous
        local("2021-10-10T18:27") -> CS(scheme = 2, period = 0, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:03") -> CS(scheme = 2, period = 0, i = 2, next = local("2021-10-10T20:04")),
        local("2021-10-10T20:07") -> CS(scheme = 2, period = 0, i = 3, next = local("2021-10-10T20:08")))),

    Day(SUNDAY, title = "Continuous, with execution time longer then cycle interval",
      date = LocalDate.parse("2021-10-10"),
      cycleDuration = 6.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-10T09:10")),
        local("2021-10-10T09:16") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-10T09:15")),
        local("2021-10-10T09:22") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:28") -> CS(scheme = 0, period = 0, i = 4, next = local("2021-10-10T10:10")),
        local("2021-10-10T10:16") -> CS(scheme = 0, period = 0, i = 5, next = local("2021-10-10T10:15")),

        local("2021-10-10T10:22") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-10T12:10")),

        // Continuous
        local("2021-10-10T12:16") -> CS(scheme = 3, period = 0, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:06") -> CS(scheme = 3, period = 0, i = 2, next = local("2021-10-10T18:11")),
        local("2021-10-10T18:17") -> CS(scheme = 3, period = 0, i = 3, next = local("2021-10-10T18:22")),

        // Continuous
        local("2021-10-10T18:28") -> CS(scheme = 2, period = 0, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:06") -> CS(scheme = 2, period = 0, i = 2, next = local("2021-10-10T20:07")),
        local("2021-10-10T20:13") -> CS(scheme = 2, period = 0, i = 3, next = local("2021-10-10T20:14")))),

    Day(SUNDAY, title = "Continuous, with execution time longer then two times cycle interval",
      date = LocalDate.parse("2021-10-10"),
      cycleDuration = 11.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T06:00") -> CS(scheme = 0, period = 0, i = 1, next = local("2021-10-10T09:10")),
        // skipped        09:21") -> CS(scheme = 0, period = 0,        next = local("2021-10-10T09:15")),
        local("2021-10-10T09:21") -> CS(scheme = 0, period = 0, i = 2, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:32") -> CS(scheme = 0, period = 0, i = 3, next = local("2021-10-10T10:10")),

        local("2021-10-10T10:21") -> CS(scheme = 0, period = 1, i = 1, next = local("2021-10-10T12:10")),

        // Continuous
        local("2021-10-10T12:21") -> CS(scheme = 3, period = 0, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:11") -> CS(scheme = 3, period = 0, i = 2, next = local("2021-10-10T18:16")),

        // Continuous
        local("2021-10-10T18:27") -> CS(scheme = 2, period = 0, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:11") -> CS(scheme = 2, period = 0, i = 2, next = local("2021-10-10T20:12")),
        local("2021-10-10T20:23") -> CS(scheme = 2, period = 0, i = 3, next = local("2021-10-10T20:24")))))

  private val onlyOnePeriodTimeOfDay = LocalTime.parse("17:00")
  assert(onlyOnePeriodTimeOfDay.toSecondOfDay > dateOffset.toSeconds)

  private val onlyOnePeriodSetting = Seq(
    Day(MONDAY,
      date = LocalDate.parse("2023-07-03"),
      expectedCycles = Seq(
        // Ticking (business day ends 6:00)
        local("2023-07-03T17:00") -> CS(scheme = 1, period = 0, i = 1, next = local("2023-07-04T02:00")),
        local("2023-07-04T02:00") -> CS(scheme = 1, period = 0, i = 2, next = local("2023-07-04T02:20")),
        local("2023-07-04T02:20") -> CS(scheme = 1, period = 0, i = 3, next = local("2023-07-04T02:40")))),

    Day(TUESDAY,
      date = LocalDate.parse("2023-07-04"),
      expectedCycles = Nil),

    Day(WEDNESDAY,
      date = LocalDate.parse("2023-07-05"),
      expectedCycles = Nil),

    Day(THURSDAY,
      date = LocalDate.parse("2023-07-06"),
      expectedCycles = Nil),

    Day(FRIDAY,
      date = LocalDate.parse("2023-07-07"),
      expectedCycles = Seq(
        // Ticking (still business calendar friday)
        local("2023-07-07T17:00") -> CS(scheme = 1, period = 1, i = 1, next = local("2023-07-08T04:00")),
        local("2023-07-08T04:00") -> CS(scheme = 1, period = 1, i = 2, next = local("2023-07-08T04:20")),
        local("2023-07-08T04:20") -> CS(scheme = 1, period = 1, i = 3, next = local("2023-07-08T04:40")))),

    Day(SATURDAY,
      date = LocalDate.parse("2023-07-08"),
      expectedCycles = Nil),

    Day(SUNDAY, title = "Continuous, with zero execution time",
      date = LocalDate.parse("2023-07-09"),
      expectedCycles = Seq(
        // Continuous
        local("2023-07-09T17:00") -> CS(scheme = 3, period = 0, i = 1, next = local("2023-07-09T18:00")),
        local("2023-07-09T18:00") -> CS(scheme = 3, period = 0, i = 2, next = local("2023-07-09T18:05")),
        local("2023-07-09T18:05") -> CS(scheme = 3, period = 0, i = 3, next = local("2023-07-09T18:10")),
        local("2023-07-09T18:10") -> CS(scheme = 3, period = 0, i = 4, next = local("2023-07-09T18:15")),
        local("2023-07-09T18:15") -> CS(scheme = 3, period = 0, i = 5, next = local("2023-07-09T18:20")),
        local("2023-07-09T18:20") -> CS(scheme = 3, period = 0, i = 6, next = local("2023-07-09T18:25")))),

    Day(SUNDAY, title = "Continuous, with execution time shorter than cycle interval",
      date = LocalDate.parse("2023-07-09"),
      cycleDuration = 3.minutes,
      expectedCycles = Seq(
        // Continuous
        local("2023-07-09T17:00") -> CS(scheme = 3, period = 0, i = 1, next = local("2023-07-09T18:00")),
        local("2023-07-09T18:03") -> CS(scheme = 3, period = 0, i = 2, next = local("2023-07-09T18:08")),
        local("2023-07-09T18:11") -> CS(scheme = 3, period = 0, i = 3, next = local("2023-07-09T18:16")),
        local("2023-07-09T18:19") -> CS(scheme = 3, period = 0, i = 4, next = local("2023-07-09T18:24")))),

    Day(SUNDAY, title = "Continuous, with execution time longer then cycle interval",
      date = LocalDate.parse("2023-07-09"),
      cycleDuration = 6.minutes,
      expectedCycles = Seq(
        // Continuous
        local("2023-07-09T17:00") -> CS(scheme = 3, period = 0, i = 1, next = local("2023-07-09T18:00")),
        local("2023-07-09T18:06") -> CS(scheme = 3, period = 0, i = 2, next = local("2023-07-09T18:11")),
        local("2023-07-09T18:17") -> CS(scheme = 3, period = 0, i = 3, next = local("2023-07-09T18:22")))),

    Day(SUNDAY, title = "Continuous, with execution time longer then two times cycle interval",
      date = LocalDate.parse("2023-07-09"),
      cycleDuration = 11.minutes,
      expectedCycles = Seq(
        // Continuous
        local("2023-07-09T17:00") -> CS(scheme = 3, period = 0, i = 1, next = local("2023-07-09T18:00")),
        local("2023-07-09T18:11") -> CS(scheme = 3, period = 0, i = 2, next = local("2023-07-09T18:16")))))

  private final case class Day(
    dayOfWeek: DayOfWeek,
    date: LocalDate,
    cycleDuration: FiniteDuration = 0.s,
    title: String = "",
    expectedCycles: Seq[(Timestamp, CS)])
  :
    def testName = dayOfWeek.toString.toLowerCase(Locale.ROOT).capitalize +
      " " + cycleDuration.pretty +
      (title.??.fold("")(" — " + _))

  private final case class CS(scheme: Int, period: Int, i: Int, next: Timestamp)
  :
    def toCycleState(until: Timestamp) =
      CycleState(until, schemeIndex = scheme, periodIndex = period, index = i, next = next)
