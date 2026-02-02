package js7.data.event

import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.EventCalcTest.*
import js7.data.event.KeyedEvent.NoKey
import scala.concurrent.duration.{Deadline, FiniteDuration}

// See also ControllerEventCalcTest //

final class EventCalcTest extends OurTestSuite:

  "Combine different Ctx to a union type" in:
    trait X:
      def xValue: String

    trait Y:
      def yValue: String

    val a: EventCalcCtx[TestState, TestEvent, Any] =
      EventCalcCtx.pure(TestEvent.Added("(Any)"))

    val b: EventCalcCtx[TestState, TestEvent.Added, X] =
      EventCalcCtx: coll =>
        coll:
          TestEvent.Added(coll.context.xValue)

    val c: EventCalcCtx[TestState, TestEvent.Added, Y] =
      EventCalcCtx: coll =>
        coll:
          TestEvent.Added(coll.context.yValue)

    val combined: EventCalcCtx[TestState, TestEvent, X & Y] =
      EventCalcCtx.combineAll(Seq(a.widen, b.widen, c.widen))

    val coll = combined.calculate:
      EventCollCtx(
        TestState("START"),
        new X with Y:
          def xValue = "(X)"
          def yValue = "(Y)")
    .orThrow

    assert(coll.keyedEventList == Seq(
        NoKey <-: TestEvent.Added("(Any)"),
        NoKey <-: TestEvent.Added("(X)"),
        NoKey <-: TestEvent.Added("(Y)")))

    assert(coll.aggregate == TestState("START(Any)(X)(Y)"))

  "combineAll" - {
    "empty sequence" in:
      val a = MyAggregate(0)
      val coll = EventCalcCtx.combineAll(Nil).calculate(a, ()).orThrow
      assert(!coll.hasEvents && (coll.aggregate eq a))

    "combineAll and Problem" in:
      val combined = EventCalcCtx.combineAll[MyAggregate, Added, Unit](List(
        EventCalcCtx.pure(Added(1)),
        EventCalcCtx.problem(Problem("PROBLEM")),
        EventCalcCtx(coll => throw new AssertionError("MUST NOT BE CALLED"))))
      assert:
        combined.calculate(EventCollCtx(MyAggregate(0))) == Left(Problem("PROBLEM"))

    "combineAll does not overflow the stack, and speed test" in:
      def run(n: Int): FiniteDuration =
        val eventCalc: EventCalcCtx[MyAggregate, Added, Any] =
          val singleEventCalc = EventCalcCtx.pure[MyAggregate, Added, Any](Added(1))
          EventCalcCtx.combineAll:
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
