package js7.data.execution.workflow.instructions

import java.time.DayOfWeek.{FRIDAY, MONDAY, SATURDAY, SUNDAY, THURSDAY, TUESDAY, WEDNESDAY}
import java.time.{DayOfWeek, ZoneId}
import java.util.Locale
import js7.base.log.Logger
import js7.base.time.JavaTimestamp.local
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime._
import js7.base.time.{TimeInterval, Timestamp}
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.execution.workflow.instructions.CycleTester._
import js7.data.order.CycleState
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

trait CycleTester extends AnyFreeSpec
{
  /** For testing the example in `js7.data.workflow.instructions.CycleTest`. */
  protected final def addStandardCycleTests(
    testDay: (TimeInterval, FiniteDuration, ZoneId, Seq[(Timestamp, CycleState)], Timestamp) => Unit)
    (implicit pos: source.Position)
  : Unit =
    for (day <- setting) {
      day.testName in {
        logger.debug("—"*40 + day.testName)
        assert(day.dayOfWeek == day.start.toLocalDateTime(zoneId).getDayOfWeek,
          "Weekday does not match start date")
        testDay(
          TimeInterval(day.start, 24.h),
          day.cycleDuration,
          zoneId,
          day.expectedCycles.map { case (now, cs) => now -> cs.toCycleState(day.end) },
          day.exit)
      }
    }
}

object CycleTester
{
  implicit val zoneId = ZoneId.of("Europe/Mariehamn")
  private val logger = Logger[this.type]

  private val setting = Seq(
    Day(MONDAY,
      start = local("2021-10-04T01:00"),
      end   = local("2021-10-05T01:00"),
      expectedCycles = Seq(
        // Ticking
        local("2021-10-04T01:00") -> CS(scheme = 1, i = 1, next = local("2021-10-04T02:00")),
        local("2021-10-04T02:00") -> CS(scheme = 1, i = 2, next = local("2021-10-04T02:20")),
        local("2021-10-04T02:20") -> CS(scheme = 1, i = 3, next = local("2021-10-04T02:40")),

        // Periodic
        local("2021-10-04T02:40") -> CS(scheme = 0, i = 1, next = local("2021-10-04T09:10")),
        local("2021-10-04T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-04T09:15")),
        local("2021-10-04T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-04T09:20")),
        local("2021-10-04T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-04T10:10")),
        local("2021-10-04T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-04T10:15"))),
      exit = local("2021-10-04T10:15")),


    Day(TUESDAY,
      start = local("2021-10-05T01:00"),
      end   = local("2021-10-06T01:00"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-05T01:00") -> CS(scheme = 0, i = 1, next = local("2021-10-05T09:10")),
        local("2021-10-05T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-05T09:15")),
        local("2021-10-05T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-05T09:20")),
        local("2021-10-05T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-05T10:10")),
        local("2021-10-05T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-05T10:15"))),
      exit = local("2021-10-05T10:15")),

    Day(WEDNESDAY,
      start = local("2021-10-06T01:00"),
      end   = local("2021-10-07T01:00"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-06T01:00") -> CS(scheme = 0, i = 1, next = local("2021-10-06T09:10")),
        local("2021-10-06T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-06T09:15")),
        local("2021-10-06T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-06T09:20")),
        local("2021-10-06T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-06T10:10")),
        local("2021-10-06T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-06T10:15"))),
      exit = local("2021-10-06T10:15")),

    Day(THURSDAY,
      start = local("2021-10-07T01:00"),
      end   = local("2021-10-08T01:00"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-07T01:00") -> CS(scheme = 0, i = 1, next = local("2021-10-07T09:10")),
        local("2021-10-07T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-07T09:15")),
        local("2021-10-07T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-07T09:20")),
        local("2021-10-07T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-07T10:10")),
        local("2021-10-07T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-07T10:15"))),
      exit = local("2021-10-07T10:15")),

    Day(FRIDAY,
      start = local("2021-10-08T01:00"),
      end   = local("2021-10-09T01:00"),
      expectedCycles = Seq(
        // Ticking
        local("2021-10-08T01:00") -> CS(scheme = 1, i = 1, next = local("2021-10-08T04:00")),
        local("2021-10-08T04:00") -> CS(scheme = 1, i = 2, next = local("2021-10-08T04:20")),
        local("2021-10-08T04:20") -> CS(scheme = 1, i = 3, next = local("2021-10-08T04:40")),

        // Periodic
        local("2021-10-08T04:40") -> CS(scheme = 0, i = 1, next = local("2021-10-08T09:10")),
        local("2021-10-08T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-08T09:15")),
        local("2021-10-08T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-08T09:20")),
        local("2021-10-08T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-08T10:10")),
        local("2021-10-08T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-08T10:15"))),
      exit = local("2021-10-08T10:15")),

    Day(SATURDAY,
      start = local("2021-10-09T01:00"),
      end   = local("2021-10-10T01:00"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-09T01:00") -> CS(scheme = 0, i = 1, next = local("2021-10-09T09:10")),
        local("2021-10-09T09:10") -> CS(scheme = 0, i = 2, next = local("2021-10-09T09:15")),
        local("2021-10-09T09:15") -> CS(scheme = 0, i = 3, next = local("2021-10-09T09:20")),
        local("2021-10-09T09:20") -> CS(scheme = 0, i = 4, next = local("2021-10-09T10:10")),
        local("2021-10-09T10:10") -> CS(scheme = 0, i = 5, next = local("2021-10-09T10:15"))),
      exit = local("2021-10-09T10:15")),

    Day(SUNDAY, title = "Continuous, with zero execution time",
      start = local("2021-10-10T01:00"),
      end   = local("2021-10-11T01:00"),
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T01:00") -> CS(scheme = 0, i = 1, next = local("2021-10-10T09:10")),
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
        local("2021-10-10T20:01") -> CS(scheme = 3, i = 3, next = local("2021-10-10T20:02"))),
      exit = local("2021-10-10T20:02")),

    Day(SUNDAY, title = "Continuous, with execution time shorter than cycle interval",
      start = local("2021-10-10T01:00"),
      end   = local("2021-10-11T01:00"),
      cycleDuration = 3.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T01:00") -> CS(scheme = 0, i = 1, next = local("2021-10-10T09:10")),
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
        local("2021-10-10T20:07") -> CS(scheme = 3, i = 3, next = local("2021-10-10T20:08"))),
      exit = local("2021-10-10T20:11")),

    Day(SUNDAY, title = "Continuous, with execution time longer then cycle interval",
      start = local("2021-10-10T01:00"),
      end   = local("2021-10-11T01:00"),
      cycleDuration = 6.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T01:00") -> CS(scheme = 0, i = 1, next = local("2021-10-10T09:10")),
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
        local("2021-10-10T20:13") -> CS(scheme = 3, i = 3, next = local("2021-10-10T20:14"))),
      exit = local("2021-10-10T20:20")),

    Day(SUNDAY, title = "Continuous, with execution time longer then two times cycle interval",
      start = local("2021-10-10T01:00"),
      end   = local("2021-10-11T01:00"),
      cycleDuration = 11.minutes,
      expectedCycles = Seq(
        // Periodic
        local("2021-10-10T01:00") -> CS(scheme = 0, i = 1, next = local("2021-10-10T09:10")),
        // skipped        09:21") -> CS(scheme = 0,        next = local("2021-10-10T09:15")),
        local("2021-10-10T09:21") -> CS(scheme = 0, i = 2, next = local("2021-10-10T09:20")),
        local("2021-10-10T09:32") -> CS(scheme = 0, i = 3, next = local("2021-10-10T10:10")),

        // Continuous
        local("2021-10-10T10:21") -> CS(scheme = 2, i = 1, next = local("2021-10-10T18:00")),
        local("2021-10-10T18:11") -> CS(scheme = 2, i = 2, next = local("2021-10-10T18:16")),

        // Continuous
        local("2021-10-10T18:27") -> CS(scheme = 3, i = 1, next = local("2021-10-10T20:00")),
        local("2021-10-10T20:11") -> CS(scheme = 3, i = 2, next = local("2021-10-10T20:12")),
        local("2021-10-10T20:23") -> CS(scheme = 3, i = 3, next = local("2021-10-10T20:24"))),
      exit = local("2021-10-10T20:35")))

  private final case class Day(
    dayOfWeek: DayOfWeek,
    start: Timestamp,
    end: Timestamp,
    cycleDuration: FiniteDuration = 0.s,
    title: String = "",
    expectedCycles: Seq[(Timestamp, CS)],
    exit: Timestamp)
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
