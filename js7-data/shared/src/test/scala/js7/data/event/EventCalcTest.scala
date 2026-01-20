package js7.data.event

import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.TimestampForTests.ts
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventCalcTest.*
import js7.data.event.KeyedEvent.NoKey
import scala.concurrent.duration.{Deadline, FiniteDuration}

// See also ControllerEventCalcTest //

final class EventCalcTest extends OurTestSuite:

  "test" in:
    trait X:
      def value: String

    val a: EventCalc[TestState, TestEvent, Any] =
      EventCalc.pure(TestEvent.Added("»"))

    val b: EventCalc[TestState, TestEvent.Added, TimeCtx] =
      EventCalc.single: _ =>
        TestEvent.Added(EventCalc.now.toString)

    val c: EventCalc[TestState, TestEvent.Added, X] =
      EventCalc.single: _ =>
        TestEvent.Added(EventCalc.context.value)

    val combined: EventCalc[TestState, TestEvent, TimeCtx & X] =
      EventCalc.combineAll(Seq(a.widen, b.widen, c.widen))

    val (events, testState) = combined.calculateEventsAndAggregate(
      TestState("START"),
      new TimeCtx with X:
        def now = ts"2025-03-13T12:00:00Z"
        def value = "«"
    ).orThrow

    assert(events == Seq(
        NoKey <-: TestEvent.Added("»"),
        NoKey <-: TestEvent.Added("2025-03-13T12:00:00Z"),
        NoKey <-: TestEvent.Added("«")))

    assert(testState == TestState("START»2025-03-13T12:00:00Z«"))

  "combineAll" - {
    "empty sequence" in:
      val a = MyAggregate(0)
      val coll = EventCalc.combineAll(Nil).calculate(a, ()).orThrow
      assert(!coll.hasEvents && (coll.aggregate eq a))

    "combineAll and Problem" in:
      val combined = EventCalc.combineAll[MyAggregate, Added, Unit](List(
        EventCalc.pure(Added(1)),
        EventCalc.problem(Problem("PROBLEM")),
        EventCalc(coll => throw new AssertionError("MUST NOT BE CALLED"))))
      assert:
        combined.calculate(EventColl(MyAggregate(0))) == Left(Problem("PROBLEM"))

    "combineAll does not overflow the stack, and speed test" in:
      def run(n: Int): FiniteDuration =
        val eventCalc: EventCalc[MyAggregate, Added, Any] =
          val singleEventCalc = EventCalc.pure[MyAggregate, Added, Any](Added(1))
          EventCalc.combineAll:
            Iterator.fill(n)(singleEventCalc)

        val t = Deadline.now
        val result = eventCalc.calculate(MyAggregate(0), ()).orThrow.aggregate
        val duration = t.elapsed
        assert(result == MyAggregate(n))
        duration

      run(10000) // warm-up

      val n = 1_000_000
      val duration = run(n)
      val line = itemsPerSecondString(duration, n, "EventCalcs")
      logger.info(line)
      info(line)
  }


object EventCalcTest:
  private val logger = Logger[this.type]

  private final case class MyAggregate(n: Int)
    extends EventDrivenState_[MyAggregate, Added]:

    def companion = MyAggregate

    def applyKeyedEvent(keyedEvent: KeyedEvent[Added]) =
      Right(copy(n = n + keyedEvent.event.i))

  private object MyAggregate extends EventDrivenState.Companion[MyAggregate]

  private final case class Added(i: Int) extends NoKeyEvent
