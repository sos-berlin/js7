package js7.data.execution.workflow.instructions

import java.time.ZoneOffset.UTC
import java.time.{LocalTime, ZoneId}
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{AdmissionTimeScheme, DailyPeriod, TimeInterval, Timestamp}
import js7.data.execution.workflow.instructions.ScheduleCalculator.Do
import js7.data.order.CycleState
import js7.data.workflow.instructions.Schedule
import js7.data.workflow.instructions.Schedule.{Periodic, Scheme, Ticking}
import scala.concurrent.duration.*

final class ScheduleCalculatorTest extends OurTestSuite, ScheduleTester:

  "Details" - {
    val schedule = Schedule(
      Seq(
        Scheme(
          AdmissionTimeScheme(Seq(DailyPeriod(LocalTime.parse("09:00"), duration = 2.h))),
          Periodic(1.h, Seq(5.minutes, 10.minutes))),
        Scheme(
          AdmissionTimeScheme(Seq(
            DailyPeriod(LocalTime.parse("15:07"), duration = 30.minutes),
            DailyPeriod(LocalTime.parse("12:00"), duration = 1.h))),
          Ticking(15.minutes))))

    val calculator = ScheduleCalculator(schedule, UTC, dateOffset = 6.h)

    "Periodic" - {
      val cs = CycleState(
        next = Timestamp.Epoch/*not used*/,
        end  = ts"2021-10-02T00:00:00Z",
        index = 0)

      "First cycle (OrderCyclingPrepared)" - {
        val initialCycleState = cs.copy(schemeIndex = -1)
        val first = cs.copy(index = 1, next = ts"2021-10-01T09:05:00Z")

        "now < first cycle start" in:
          assert(calculator.nextCycleState(initialCycleState, ts"2021-10-01T08:00:00Z") ==
            Some(first))

        "now == first cycle start" in:
          assert(calculator.nextCycleState(initialCycleState, ts"2021-10-01T09:05:00Z") ==
            Some(first))

        "now > first cycle start" in:
          assert(calculator.nextCycleState(initialCycleState, ts"2021-10-01T09:06:00Z") ==
            Some(first))

        "now == second cycle start" in:
          assert(calculator.nextCycleState(initialCycleState, ts"2021-10-01T09:10:00Z") ==
            Some(first.copy(
              next = ts"2021-10-01T09:10:00Z")))

        "now > second cycle start" in:
          assert(calculator.nextCycleState(initialCycleState, ts"2021-10-01T09:59:00Z") ==
            Some(first.copy(
              next = ts"2021-10-01T09:10:00Z")))

        "now >> second cycle start" in:
          assert(calculator.nextCycleState(initialCycleState, ts"2021-10-01T10:05:00Z") ==
            Some(first.copy(
              next = ts"2021-10-01T10:05:00Z")))
          assert(calculator.nextCycleState(initialCycleState, ts"2021-10-01T10:06:00Z") ==
            Some(first.copy(
              next = ts"2021-10-01T10:05:00Z")))
      }

      "Next cycle (OrderCycleFinished)" - {
        val last = cs.copy(index = 1, next = ts"2021-10-01T09:05:00Z")
        val next = cs.copy(index = 2, next = ts"2021-10-01T09:10:00Z")

        "now < first (only if clock has been manipulated)" in:
          assert(calculator.nextCycleState(last, ts"2021-10-01T08:00:00Z") == Some(next))

        "now == last next (zero execution time)" in:
          assert(calculator.nextCycleState(last, ts"2021-10-01T09:05:00Z") == Some(next))

        "now < next" in:
          assert(calculator.nextCycleState(last, ts"2021-10-01T09:09:00Z") == Some(next))

        "now == next" in:
          assert(calculator.nextCycleState(last, ts"2021-10-01T09:10:00Z") == Some(next))

        "now > next" in:
          assert(calculator.nextCycleState(last, ts"2021-10-01T09:30:00Z") == Some(next))

        "now >> next" in:
          assert(calculator.nextCycleState(last, ts"2021-10-01T10:05:00Z") ==
            Some(next.copy(next = ts"2021-10-01T10:05:00Z")))
          assert(calculator.nextCycleState(last, ts"2021-10-01T10:06:00Z") ==
            Some(next.copy(next = ts"2021-10-01T10:05:00Z")))

        "now >>> next" in:
          assert(calculator.nextCycleState(last, ts"2021-10-01T10:10:00Z") ==
            Some(next.copy(next = ts"2021-10-01T10:10:00Z")))
          assert(calculator.nextCycleState(last, ts"2021-10-01T10:11:00Z") ==
            Some(next.copy(next = ts"2021-10-01T10:10:00Z")))

        "At end of this scheme, change to next 12:00 Ticking scheme" in:
          val last = next.copy(next = ts"2021-10-01T10:10:00Z")
          assert(calculator.nextCycleState(last, ts"2021-10-01T10:11:00Z") ==
            Some(cs.copy(next = ts"2021-10-01T12:00:00Z", schemeIndex = 1, periodIndex = 1, index = 1)))
      }
    }

    "Ticking" - {
      val cycleState0 = CycleState(
        next = Timestamp.Epoch/*not used*/,
        end  = ts"2021-10-02T00:00:00Z",
        schemeIndex = 1,
        periodIndex = 1,
        index = 0)

      "First cycle (OrderCyclingPrepared)" - {
        val initialCycleState = cycleState0.copy(schemeIndex = -1)

        "now < first" in:
          val cs = calculator.nextCycleState(initialCycleState, ts"2021-10-01T11:00:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:00:00Z", index = 1))
          assert(calculator.onNextCycleIsDue(cs, now = ts"2021-10-01T11:59:59Z") ==
            Right(Do.KeepWaiting))
          assert(calculator.onNextCycleIsDue(cs, now = ts"2021-10-01T12:00:00Z") ==
            Right(Do.StartCycle()))

        "now == first" in:
          val cs = calculator.nextCycleState(initialCycleState, ts"2021-10-01T12:00:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:00:00Z", index = 1))

        "now > first" in:
          val cs = calculator.nextCycleState(initialCycleState, ts"2021-10-01T12:01:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:00:00Z", index = 1))

        "now >> first" in:
          val cs = calculator.nextCycleState(initialCycleState, ts"2021-10-01T12:11:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:00:00Z", index = 1))

        "now >>> first" in:
          val cs = calculator.nextCycleState(initialCycleState, ts"2021-10-01T12:59:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:45:00Z", index = 1))

      }

      "Second cycle (OrderCycleFinished)"/*Warum OrderCycleFinished???*/ - {
        val last = cycleState0.copy(next = ts"2021-10-01T12:00:00Z", index = 1)

        "now < first (only if clock has been manipulated)" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T11:00:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:15:00Z", index = 2))

        "now == last next (zero execution time)" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T12:00:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:15:00Z", index = 2))

        "now < next" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T12:14:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:15:00Z", index = 2))

        "now == next" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T12:15:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:15:00Z", index = 2))

        "now > next" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T12:29:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:15:00Z", index = 2))

        "now >> next (1)" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T12:30:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:30:00Z", index = 2))

        "now >> next (2)" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T12:31:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:30:00Z", index = 2))

        "now >>> next (1)" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T12:45:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:45:00Z", index = 2))

        "now >>> next (2)" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T12:46:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T12:45:00Z", index = 2))

        "Second AdmissionPeriod (in same second Scheme)" in:
          val cs = calculator.nextCycleState(last, ts"2021-10-01T13:00:00Z").get
          assert(cs == cycleState0.copy(next = ts"2021-10-01T15:07:00Z", periodIndex = 0, index = 1))
      }

      "onNextCycleIsDue" - {
        val cs = cycleState0.copy(next = ts"2021-10-01T12:15:00Z", index = 1)

        "Too early OrderCycleStarted" in :
          assert(calculator.onNextCycleIsDue(cs, ts"2021-10-01T12:10:00Z") ==
            Right(Do.KeepWaiting))

        "Just in time OrderCycleStarted" in :
          assert(calculator.onNextCycleIsDue(cs, ts"2021-10-01T12:15:00Z") ==
            Right(Do.StartCycle()))

        "Late OrderCycleStarted" in :
          assert(calculator.onNextCycleIsDue(cs, ts"2021-10-01T12:29:59Z") ==
            Right(Do.StartCycle(None)))

        "Late OrderCycleStarted, missing one tick" in :
          assert(calculator.onNextCycleIsDue(cs, ts"2021-10-01T12:30:00Z") ==
            Right(Do.StartCycle(skipped = Some(15.minute))))

        "Late OrderCycleStarted, missing five ticks" in :
          assert(calculator.onNextCycleIsDue(cs, ts"2021-10-01T12:45:00Z") ==
            Right(Do.StartCycle(skipped = Some(2 * 15.minutes))))

        "End of Ticking period" in :
          assert(calculator.onNextCycleIsDue(cs, ts"2021-10-01T13:00:00Z") ==
            Right(Do.ChangeCycleState(cs.copy(
              periodIndex = 0,
              next = ts"2021-10-01T15:07:00Z"))))
      }
    }
  }

  "Daylight saving time change" - {
    given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")

    val schedule = Schedule(Seq(Scheme(
      AdmissionTimeScheme.always,
      Periodic(1.h, Seq(0.minute, 30.minute)))))

    val calculator = ScheduleCalculator(schedule, zoneId, dateOffset = 0.s)

    "Winter to summer" in:
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
        ts"2021-03-28T01:00:00Z",
        ts"2021-03-28T01:30:00Z",
        ts"2021-03-28T02:00:00Z"))

    "Summer to winter" in:
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
        ts"2021-10-31T00:00:00Z",
        ts"2021-10-31T00:30:00Z",
        ts"2021-10-31T02:00:00Z"))  // An hour is skipped!!!
  }

  "ScheduleTester standard example" - {
    addStandardScheduleTests: (timeInterval, cycleDuration, zone, expected, onlyOnePeriod) =>
      import ScheduleTester.{dateOffset, schedule}
      val result = ScheduleCalculator(schedule, zone, dateOffset = dateOffset, onlyOnePeriod = onlyOnePeriod)
        .simulate(timeInterval, actionDuration = cycleDuration)
        .map(scheduled => scheduled.arrival -> scheduled.cycleState)
        .toSeq
      assert(result == expected)
  }

  "CycleState.empty" in:
    // CycleState.empty is only used when throwing an Order into a cycle block,
    // with a "cycle" BranchId without parameters. This should not be possible.
    implicit val zone: ZoneId = ZoneId.of("Europe/Mariehamn")
    val schedule = Schedule(Seq(Scheme(
      AdmissionTimeScheme.always,
      Periodic(1.h, Seq(1.minute)))))
    assert(ScheduleCalculator(schedule, zone, dateOffset = 0.s)
      .simulateWithCycleState(CycleState.empty, local("2024-03-25T12:00"), 24.h)
      .isEmpty)
