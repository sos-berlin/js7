package js7.data.event

import js7.base.problem.Checked.Ops
import js7.base.time.Timestamp
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.EventDrivenStateTest._
import js7.data.event.KeyedEvent.NoKey
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventDrivenStateTest extends AnyFreeSpec
{
  private var s = TestState("")

  "applyEvents" in {
    val stampedEvents =
      Stamped(1000L, Timestamp.ofEpochMilli(1), (NoKey <-: Added("("))) ::
      Stamped(2000L, Timestamp.ofEpochMilli(2), (NoKey <-: Added("ADDED"))) ::
      Stamped(3000L, Timestamp.ofEpochMilli(3), (NoKey <-: Added(")"))) ::
      Nil

    assert(s.applyStampedEvents(stampedEvents) == s.applyEvents(stampedEvents.map(_.value)))
    s = s.applyStampedEvents(stampedEvents).orThrow

    assert(s == TestState("(ADDED)"))
  }

  "EventNotApplicableProblem" in {
    assert(s.applyEvent(InvalidEvent) == Left(EventNotApplicableProblem(InvalidEvent, s)))

    val stampedEvents =
      Stamped(4000L, Timestamp.ofEpochMilli(4), (NoKey <-: Added("MORE"))) ::
      Stamped(5000L, Timestamp.ofEpochMilli(5), (NoKey <-: InvalidEvent)) ::
      Nil

    assert(s.applyStampedEvents(stampedEvents) ==
      Left(
        EventNotApplicableProblem(InvalidEvent, TestState("(ADDED)MORE"))
          withPrefix "Event 'Stamped(5000 1970-01-01T00:00:00.005Z InvalidEvent)' cannot be applied:"))
  }
}

private object EventDrivenStateTest
{
  private sealed trait TestEvent extends NoKeyEvent
  private case class Added(string: String) extends TestEvent
  private case object InvalidEvent extends TestEvent

  private case class TestState(string: String)
  extends EventDrivenState[TestState, TestEvent]
  {
    def applyEvent(keyedEvent: KeyedEvent[TestEvent]) =
      keyedEvent match {
        case KeyedEvent(NoKey, Added(s)) =>
          Right(copy(string = string + s))
        case _ =>
          eventNotApplicable(keyedEvent)
      }
  }
}
