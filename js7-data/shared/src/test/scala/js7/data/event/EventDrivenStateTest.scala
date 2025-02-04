package js7.data.event

import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.TestEvent.{Added, InvalidEvent}

/**
  * @author Joacim Zschimmer
  */
final class EventDrivenStateTest extends OurTestSuite:
  private var s = TestState("")

  "applyKeyedEvents" in:
    val stampedEvents =
      Stamped(1000L, Timestamp.ofEpochMilli(1), NoKey <-: Added("(")) ::
      Stamped(2000L, Timestamp.ofEpochMilli(2), NoKey <-: Added("ADDED")) ::
      Stamped(3000L, Timestamp.ofEpochMilli(3), NoKey <-: Added(")")) ::
      Nil
    val keyedEvents = stampedEvents.map(_.value)

    assert(s.applyKeyedEvents(keyedEvents) == s.applyKeyedEvents(keyedEvents))
    s = s.applyKeyedEvents(keyedEvents).orThrow

    assert(s == TestState("(ADDED)"))

  "EventNotApplicableProblem" in:
    assert(s.applyKeyedEvent(InvalidEvent) == Left(EventNotApplicableProblem(InvalidEvent, s)))

    val stampedEvents =
      Stamped(4000L, Timestamp.ofEpochMilli(4), NoKey <-: Added("MORE")) ::
      Stamped(5000L, Timestamp.ofEpochMilli(5), NoKey <-: InvalidEvent) ::
      Nil

    assert(s.applyKeyedEvents(stampedEvents.map(_.value)) ==
      Left(
        EventNotApplicableProblem(InvalidEvent, TestState("(ADDED)MORE"))
          .withPrefix("Event 'InvalidEvent' cannot be applied to TestCase:")))
