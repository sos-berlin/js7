package js7.data.execution.workflow.instructions

import java.time.ZoneOffset.UTC
import java.time.{LocalTime, ZoneId}
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime._
import js7.base.time.{AdmissionTimeScheme, AlwaysPeriod, DailyPeriod, TimeInterval, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.order.CycleState
import js7.data.workflow.instructions.Schedule
import js7.data.workflow.instructions.Schedule.{Periodic, Scheme, Ticking}
import js7.data.workflow.instructions.ScheduleTest.exampleSchedule
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

final class ScheduleCalculatorTest extends AnyFreeSpec with CycleTester
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

    val calculator = ScheduleCalculator.checked(schedule, UTC).orThrow

    "Periodic" - {
      val cs = CycleState(
        next = Timestamp.Epoch/*not used*/,
        end  = Timestamp("2021-10-02T00:00:00Z"),
        index = 0,
        schemeIndex = 0)

      "First cycle (OrderCyclingPrepared)" - {
        val initialCycleState = cs.copy(schemeIndex = -1)
        val first = cs.copy(next = Timestamp("2021-10-01T09:05:00Z"), index = 1)

        "now < first" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T08:00:00Z")) ==
            Some(first))
        }

        "now == first" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T09:05:00Z")) ==
            Some(first))
        }

        "now > first" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T09:06:00Z")) ==
            Some(first))
        }

        "now == second" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T09:10:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T09:10:00Z"), index = 1)))
        }

        "now > second" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T09:59:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T09:10:00Z"), index = 1)))
        }

        "now >>> first" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T10:05:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T10:05:00Z"), index = 1)))
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T10:06:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T10:05:00Z"), index = 1)))
        }
      }

      "Next cycle (OrderCycleFinished)" - {
        val last = cs.copy(next = Timestamp("2021-10-01T09:05:00Z"), index = 1)
        val next = cs.copy(next = Timestamp("2021-10-01T09:10:00Z"), index = 2)

        "now < first (only if clock has been manipulated)" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T08:00:00Z")) == Some(next))
        }

        "now == last next (zero execution time)" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T09:05:00Z")) == Some(next))
        }

        "now < next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T09:09:00Z")) == Some(next))
        }

        "now == next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T09:10:00Z")) == Some(next))
        }

        "now > next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T09:30:00Z")) == Some(next))
        }

        "now >> next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T10:05:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T10:05:00Z"), index = 2)))
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T10:06:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T10:05:00Z"), index = 2)))
        }

        "now >>> next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T10:10:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T10:10:00Z"), index = 2)))
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T10:11:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T10:10:00Z"), index = 2)))
        }

        "At end of this scheme, change to next 12:00 Ticking scheme" in {
          val last = cs.copy(next = Timestamp("2021-10-01T10:10:00Z"), index = 2)
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T10:11:00Z")) ==
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
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T11:00:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }

        "now == first" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T12:00:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }

        "now > first" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T12:01:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }

        "now >> first" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T12:11:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }

        "now >>> first" in {
          assert(calculator.nextCycleState(initialCycleState, Timestamp("2021-10-01T12:59:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)))
        }
      }

      "Next cycle (OrderCycleFinished)" - {
        val last = cs.copy(next = Timestamp("2021-10-01T12:00:00Z"), index = 1)

        "now < first (only if clock has been manipulated)" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T11:00:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now == last next (zero execution time)" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T12:00:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now < next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T12:14:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now == next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T12:15:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now > next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T12:29:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:15:00Z"), index = 2)))
        }

        "now >> next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T12:30:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:30:00Z"), index = 2)))
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T12:31:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:30:00Z"), index = 2)))
        }

        "now >>> next" in {
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T12:45:00Z")) ==
            Some(cs.copy(next = Timestamp("2021-10-01T12:45:00Z"), index = 2)))
          assert(calculator.nextCycleState(last, Timestamp("2021-10-01T12:46:00Z")) ==
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

    val calculator = ScheduleCalculator.checked(schedule, zone).orThrow

    "Winter to summer" in {
      val times = calculator
        .simulate(TimeInterval(local("2021-03-28T02:30"), 24.h), 3)
        .scheduledSeq.map(_.next)

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
        .simulate(TimeInterval(local("2021-10-31T02:30"), 24.h), 3)
        .scheduledSeq.map(_.next)

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

  "CycleTest example" - {
    addStandardCycleTests { (timeInterval, cycleDuration, zone, expected, exitTimestamp) =>
      val result =
        ScheduleCalculator(exampleSchedule, zone)
          .simulate(timeInterval, limit = 1000, jobExecutionTime = cycleDuration)
      assert(result.scheduledSeq
        .map(scheduled => scheduled.arriveAt -> scheduled.cycleState)
        == expected)
      assert(result.exitAt == exitTimestamp)
    }
  }
}
