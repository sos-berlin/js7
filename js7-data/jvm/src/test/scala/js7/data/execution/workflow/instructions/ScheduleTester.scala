package js7.data.execution.workflow.instructions

import java.time.DayOfWeek.{FRIDAY, MONDAY, SATURDAY, SUNDAY, THURSDAY, TUESDAY, WEDNESDAY}
import java.time.LocalTime.MIDNIGHT
import java.time.{DayOfWeek, LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import java.util.Locale
import js7.base.time.JavaTimeConverters.RichZonedDateTime
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.{TimeInterval, Timestamp}
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.execution.workflow.instructions.ScheduleTester.*
import js7.data.order.CycleState
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.*
import scala.jdk.DurationConverters.ScalaDurationOps

trait ScheduleTester extends AnyFreeSpec
{
  /** For testing the example in `js7.data.workflow.instructions.CycleTest`. */
  protected final def addStandardScheduleTests(
    testDay: (TimeInterval, FiniteDuration, ZoneId, Seq[(Timestamp, CycleState)]) => Unit)
    (implicit pos: source.Position)
  : Unit =
    for (day <- setting) {
      day.testName in {
        assert(day.dayOfWeek == day.date.getDayOfWeek, "Weekday does not match start date")
        val localStart = LocalDateTime.of(day.date, MIDNIGHT).plus(dateOffset.toJava)
        val localEnd = localStart.plusDays(1)
        val start = ZonedDateTime.of(localStart, zone).toTimestamp
        val end = ZonedDateTime.of(localEnd, zone).toTimestamp
        testDay(
          start -> end,
          day.cycleDuration,
          zone,
          day.expectedCycles.map { case (now, cs) => now -> cs.toCycleState(end) })
      }
    }
}

object ScheduleTester
{
  implicit val zone: ZoneId = ZoneId.of("Europe/Mariehamn")
  val dateOffset = 6.h  // Business day starts at 6:00 (i.e., switching from monday to tuesday)

  private val setting = Seq(
    Day(MONDAY,
      date = LocalDate.parse("2021-10-04"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-04T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-04T09:10")),
        local("2021-10-04T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-04T09:15")),
        local("2021-10-04T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-04T09:20")),
        local("2021-10-04T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-04T10:10")),
        local("2021-10-04T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-04T10:15")),

        // Ticking (business day ends 6:00)
        local("2021-10-04T10:15") -> CS(scheme = 1, i = 1, next = local("2021-10-05T02:00")),
        local("2021-10-05T02:00") -> CS(scheme = 1, i = 2, next = local("2021-10-05T02:20")),
        local("2021-10-05T02:20") -> CS(scheme = 1, i = 3, next = local("2021-10-05T02:40")))),


    Day(TUESDAY,
      date = LocalDate.parse("2021-10-05"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-05T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-05T09:10")),
        local("2021-10-05T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-05T09:15")),
        local("2021-10-05T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-05T09:20")),
        local("2021-10-05T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-05T10:10")),
        local("2021-10-05T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-05T10:15")))),

    Day(WEDNESDAY,
      date = LocalDate.parse("2021-10-06"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-06T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-06T09:10")),
        local("2021-10-06T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-06T09:15")),
        local("2021-10-06T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-06T09:20")),
        local("2021-10-06T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-06T10:10")),
        local("2021-10-06T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-06T10:15")))),

    Day(THURSDAY,
      date = LocalDate.parse("2021-10-07"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-07T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-07T09:10")),
        local("2021-10-07T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-07T09:15")),
        local("2021-10-07T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-07T09:20")),
        local("2021-10-07T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-07T10:10")),
        local("2021-10-07T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-07T10:15")))),

    Day(FRIDAY,
      date = LocalDate.parse("2021-10-08"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-08T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-08T09:10")),
        local("2021-10-08T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-08T09:15")),
        local("2021-10-08T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-08T09:20")),
        local("2021-10-08T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-08T10:10")),
        local("2021-10-08T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-08T10:15")),

        // Ticking (still business calendar friday)
        local("2021-10-08T10:15") -> CS(scheme = 1, i = 1, next = local("2021-10-09T04:00")),
        local("2021-10-09T04:00") -> CS(scheme = 1, i = 2, next = local("2021-10-09T04:20")),
        local("2021-10-09T04:20") -> CS(scheme = 1, i = 3, next = local("2021-10-09T04:40")))),

    Day(SATURDAY,
      date = LocalDate.parse("2021-10-09"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-09T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-09T09:10")),
        local("2021-10-09T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-09T09:15")),
        local("2021-10-09T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-09T09:20")),
        local("2021-10-09T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-09T10:10")),
        local("2021-10-09T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-09T10:15")))),

    Day(SUNDAY, title = "Continuous, with zero execution time",
      date = LocalDate.parse("2021-10-10"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-10T09:10")),
        local("2021-10-10T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-10T09:15")),
        local("2021-10-10T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-10T10:10")),
        local("2021-10-10T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-10T10:15")),

        // Continuous
        local("2021-10-10T10:15") -> CS(scheme = 2, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:00") -> CS(scheme = 2, i = 2, next = local("2021-10-10T18:05")),
        local("2021-10-10T18:05") -> CS(scheme = 2, i = 3, next = local("2021-10-10T18:10")),
        local("2021-10-10T18:10") -> CS(scheme = 2, i = 4, next = local("2021-10-10T18:15")),
        local("2021-10-10T18:15") -> CS(scheme = 2, i = 5, next = local("2021-10-10T18:20")),
        local("2021-10-10T18:20") -> CS(scheme = 2, i = 6, next = local("2021-10-10T18:25")),

        // Continuous
        local("2021-10-10T18:25") -> CS(scheme = 3, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:00") -> CS(scheme = 3, i = 2, next = local("2021-10-10T20:01")),
        local("2021-10-10T20:01") -> CS(scheme = 3, i = 3, next = local("2021-10-10T20:02")))),

    Day(SUNDAY, title = "Continuous, with execution time shorter than cycle interval",
      date = LocalDate.parse("2021-10-10"),
      cycleDuration = 3.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-10T09:10")),
        local("2021-10-10T09:13") -> CS(scheme = 0, i = 2, next = local("2021-10-10T09:15")),
        local("2021-10-10T09:18") -> CS(scheme = 0, i = 3, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:23") -> CS(scheme = 0, i = 4, next = local("2021-10-10T10:10")),
        local("2021-10-10T10:13") -> CS(scheme = 0, i = 5, next = local("2021-10-10T10:15")),

        // Continuous
        local("2021-10-10T10:18") -> CS(scheme = 2, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:03") -> CS(scheme = 2, i = 2, next = local("2021-10-10T18:08")),
        local("2021-10-10T18:11") -> CS(scheme = 2, i = 3, next = local("2021-10-10T18:16")),
        local("2021-10-10T18:19") -> CS(scheme = 2, i = 4, next = local("2021-10-10T18:24")),

        // Continuous
        local("2021-10-10T18:27") -> CS(scheme = 3, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:03") -> CS(scheme = 3, i = 2, next = local("2021-10-10T20:04")),
        local("2021-10-10T20:07") -> CS(scheme = 3, i = 3, next = local("2021-10-10T20:08")))),

    Day(SUNDAY, title = "Continuous, with execution time longer then cycle interval",
      date = LocalDate.parse("2021-10-10"),
      cycleDuration = 6.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-10T09:10")),
        local("2021-10-10T09:16") -> CS(scheme = 0, i = 2, next = local("2021-10-10T09:15")),
        local("2021-10-10T09:22") -> CS(scheme = 0, i = 3, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:28") -> CS(scheme = 0, i = 4, next = local("2021-10-10T10:10")),
        local("2021-10-10T10:16") -> CS(scheme = 0, i = 5, next = local("2021-10-10T10:15")),

        // Continuous
        local("2021-10-10T10:22") -> CS(scheme = 2, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:06") -> CS(scheme = 2, i = 2, next = local("2021-10-10T18:11")),
        local("2021-10-10T18:17") -> CS(scheme = 2, i = 3, next = local("2021-10-10T18:22")),

        // Continuous
        local("2021-10-10T18:28") -> CS(scheme = 3, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:06") -> CS(scheme = 3, i = 2, next = local("2021-10-10T20:07")),
        local("2021-10-10T20:13") -> CS(scheme = 3, i = 3, next = local("2021-10-10T20:14")))),

    Day(SUNDAY, title = "Continuous, with execution time longer then two times cycle interval",
      date = LocalDate.parse("2021-10-10"),
      cycleDuration = 11.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T06:00") -> CS(scheme = 0, i = 1, next = local("2021-10-10T09:10")),
        // skipped        09:21") -> CS(scheme = 0,        next = local("2021-10-10T09:15")),
        local("2021-10-10T09:21") -> CS(scheme = 0, i = 2, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:32") -> CS(scheme = 0, i = 3, next = local("2021-10-10T10:10")),

        // Continuous
        local("2021-10-10T10:21") -> CS(scheme = 2, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:11") -> CS(scheme = 2, i = 2, next = local("2021-10-10T18:16")),

        // Continuous
        local("2021-10-10T18:27") -> CS(scheme = 3, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:11") -> CS(scheme = 3, i = 2, next = local("2021-10-10T20:12")),
        local("2021-10-10T20:23") -> CS(scheme = 3, i = 3, next = local("2021-10-10T20:24")))))

  private final case class Day(
    dayOfWeek: DayOfWeek,
    date: LocalDate,
    cycleDuration: FiniteDuration = 0.s,
    title: String = "",
    expectedCycles: Seq[(Timestamp, CS)])
  {
    def testName = dayOfWeek.toString.toLowerCase(Locale.ROOT).capitalize +
      " " + cycleDuration.pretty +
      (title.??.fold("")(" — " + _))
  }

  private final case class CS(scheme: Int, i: Int, next: Timestamp)
  {
    def toCycleState(until: Timestamp) =
      CycleState(until, schemeIndex = scheme, index = i, next = next)
  }
}
