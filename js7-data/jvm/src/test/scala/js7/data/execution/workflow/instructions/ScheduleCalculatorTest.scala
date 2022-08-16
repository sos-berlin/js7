package js7.data.execution.workflow.instructions

import java.time.ZoneOffset.UTC
import java.time.{LocalTime, ZoneId}
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.test.Test
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime._
import js7.base.time.{AdmissionTimeScheme, AlwaysPeriod, DailyPeriod, TimeInterval, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.order.CycleState
import js7.data.workflow.instructions.Schedule
import js7.data.workflow.instructions.Schedule.{Periodic, Scheme, Ticking}
import js7.data.workflow.instructions.ScheduleTest.exampleSchedule
import scala.concurrent.duration._

final class ScheduleCalculatorTest extends Test with ScheduleTester
{
  coupleScribeWithSlf4j()

  "Details" - {
    val schedule = Schedule(
      Seq(
        Scheme(
          AdmissionTimeScheme(Seq(DailyPeriod(LocalTime.parse("09:00"), duration = 2.h))),
          Periodic(1.h, Seq(5.minutes, 10.minutes))),
        Scheme(
          AdmissionTimeScheme(Seq(DailyPeriod(LocalTime.parse("12:00"), duration = 1.h))),
          Ticking(15.minutes))))

    val calculator = ScheduleCalculator.checked(schedule, UTC, dateOffset = 6.h).orThrow

    "Periodic" - {
      val cs = CycleState(
        next = Timestamp.Epoch/*not used*/,
        end  = Timestamp("2021-10-02T00:00:00Z"),
        index = 0)

      "First cycle (OrderCyclingPrepared)" - {
        val initialCycleState = cs.copy(schemeIndex = -1)
        val first = cs.copy(index = 1, next = Timestamp("2021-10-01T09:05:00Z"))

        "now < first cycle start" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T08:00:00Z"), initialCycleState) ==
            Some(first))
        }

        "now == first cycle start" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T09:05:00Z"), initialCycleState) ==
            Some(first))
        }

        "now > first cycle start" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T09:06:00Z"), initialCycleState) ==
            Some(first))
        }

        "now == second cycle start" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T09:10:00Z"), initialCycleState) ==
            Some(first.copy(
              next = Timestamp("2021-10-01T09:10:00Z"))))
        }

        "now > second cycle start" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T09:59:00Z"), initialCycleState) ==
            Some(first.copy(
              next = Timestamp("2021-10-01T09:10:00Z"))))
        }

        "now >> second cycle start" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T10:05:00Z"), initialCycleState) ==
            Some(first.copy(
              next = Timestamp("2021-10-01T10:05:00Z"))))
          assert(calculator.nextCycleState(Timestamp("2021-10-01T10:06:00Z"), initialCycleState) ==
            Some(first.copy(
              next = Timestamp("2021-10-01T10:05:00Z"))))
        }
      }

      "Next cycle (OrderCycleFinished)" - {
        val last = cs.copy(index = 1, next = Timestamp("2021-10-01T09:05:00Z"))
        val next = cs.copy(index = 2, next = Timestamp("2021-10-01T09:10:00Z"))

        "now < first (only if clock has been manipulated)" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T08:00:00Z"), last) == Some(next))
        }

        "now == last next (zero execution time)" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T09:05:00Z"), last) == Some(next))
        }

        "now < next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T09:09:00Z"), last) == Some(next))
        }

        "now == next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T09:10:00Z"), last) == Some(next))
        }

        "now > next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T09:30:00Z"), last) == Some(next))
        }

        "now >> next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T10:05:00Z"), last) ==
            Some(next.copy(next = Timestamp("2021-10-01T10:05:00Z"))))
          assert(calculator.nextCycleState(Timestamp("2021-10-01T10:06:00Z"), last) ==
            Some(next.copy(next = Timestamp("2021-10-01T10:05:00Z"))))
        }

        "now >>> next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T10:10:00Z"), last) ==
            Some(next.copy(next = Timestamp("2021-10-01T10:10:00Z"))))
          assert(calculator.nextCycleState(Timestamp("2021-10-01T10:11:00Z"), last) ==
            Some(next.copy(next = Timestamp("2021-10-01T10:10:00Z"))))
        }

        "At end of this scheme, change to next 12:00 Ticking scheme" in {
          val last = next.copy(next = Timestamp("2021-10-01T10:10:00Z"))
          assert(calculator.nextCycleState(Timestamp("2021-10-01T10:11:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1, schemeIndex = 1)))
        }
      }
    }

    "Ticking" - {
      val cs = CycleState(
        next = Timestamp.Epoch/*not used*/,
        end  = Timestamp("2021-10-02T00:00:00Z"),
        index = 0,
        schemeIndex = 1)

      "First cycle (OrderCyclingPrepared)" - {
        val initialCycleState = cs.copy(schemeIndex = -1)

        "now < first" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T11:00:00Z"), initialCycleState) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }

        "now == first" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:00:00Z"), initialCycleState) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }

        "now > first" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:01:00Z"), initialCycleState) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }

        "now >> first" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:11:00Z"), initialCycleState) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }

        "now >>> first" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:59:00Z"), initialCycleState) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:45:00Z"), index = 1)))
        }
      }

      "Next cycle (OrderCycleFinished)" - {
        val last = cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)

        "now < first (only if clock has been manipulated)" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T11:00:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now == last next (zero execution time)" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:00:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now < next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:14:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now == next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:15:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now > next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:29:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now >> next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:30:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:30:00Z"), index = 2)))
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:31:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:30:00Z"), index = 2)))
        }

        "now >>> next" in {
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:45:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:45:00Z"), index = 2)))
          assert(calculator.nextCycleState(Timestamp("2021-10-01T12:46:00Z"), last) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:45:00Z"), index = 2)))
        }
      }
    }
  }

  "Daylight saving time change" - {
    implicit val zone = ZoneId.of("Europe/Mariehamn")

    val schedule = Schedule(Seq(Scheme(
      AdmissionTimeScheme(Seq(
        AlwaysPeriod)),
      Periodic(1.h, Seq(0.minute, 30.minute)))))

    val calculator = ScheduleCalculator(schedule, zone, dateOffset = 0.s)

    "Winter to summer" in {
      val times = calculator
        .simulate(TimeInterval(local("2021-03-28T02:30"), 24.h))
        .take(3)
        .map(_.next)
        .toSeq

      assert(times == Seq(
        local("2021-03-28T03:00"),
        local("2021-03-28T03:30"),
        local("2021-03-28T05:00")))  // Local clock has skipped an hour

      assert(times == Seq(
        Timestamp("2021-03-28T01:00:00Z"),
        Timestamp("2021-03-28T01:30:00Z"),
        Timestamp("2021-03-28T02:00:00Z")))
    }

    "Summer to winter" in {
      val times = calculator
        .simulate(TimeInterval(local("2021-10-31T02:30"), 24.h))
        .take(3)
        .map(_.next)
        .toSeq

      assert(times == Seq(
        local("2021-10-31T03:00"),
        local("2021-10-31T03:30"),
        local("2021-10-31T04:00")))

      assert(times == Seq(
        Timestamp("2021-10-31T00:00:00Z"),
        Timestamp("2021-10-31T00:30:00Z"),
        Timestamp("2021-10-31T02:00:00Z")))  // An hour is skipped!!
    }
  }

  "ScheduleTest example" - {
    addStandardScheduleTests { (timeInterval, cycleDuration, zone, expected) =>
      val result =
        ScheduleCalculator(exampleSchedule, zone, dateOffset = ScheduleTester.dateOffset)
          .simulate(timeInterval, actionDuration = cycleDuration)
      assert(result
        .map(scheduled => scheduled.arrival -> scheduled.cycleState)
        .toSeq
        == expected)
    }
  }
}
