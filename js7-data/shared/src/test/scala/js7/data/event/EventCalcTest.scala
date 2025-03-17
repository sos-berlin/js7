package js7.data.event

import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.KeyedEvent.NoKey

// See also ControllerEventCalcTest //

final class EventCalcTest extends OurTestSuite:

  "test" in:
    trait X:
      def value: String

    val a: EventCalc[TestState, TestEvent, Any] =
      EventCalc.add(TestEvent.Added("»"))

    val b: EventCalc[TestState, TestEvent.Added, TimeCtx] =
      EventCalc: coll =>
        coll.add(TestEvent.Added(coll.context.now.toString))

    val c: EventCalc[TestState, TestEvent.Added, X] =
      EventCalc: coll =>
        coll.add(TestEvent.Added(coll.context.value))

    val combined: EventCalc[TestState, TestEvent, TimeCtx & X] =
      EventCalc.combine(a.widen, b.widen, c.widen)

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
