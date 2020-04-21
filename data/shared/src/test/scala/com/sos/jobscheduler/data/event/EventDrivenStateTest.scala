package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.data.event.EventDrivenStateTest._
import com.sos.jobscheduler.data.event.JournaledState.EventNotApplicableProblem
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventDrivenStateTest extends AnyFreeSpec
{
  private var s = TestState("")

  "applyEvents" in {
    s = s.applyEvents(
      (NoKey <-: AddEvent("(")) ::
      (NoKey <-: AddEvent("ADDED")) ::
      (NoKey <-: AddEvent(")")) ::
      Nil
    ).orThrow

    assert(s == TestState("(ADDED)"))
  }

  "EventNotApplicableProblem" in {
    assert(s.applyEvent(InvalidEvent) == Left(EventNotApplicableProblem(InvalidEvent, s)))
  }
}

private object EventDrivenStateTest
{
  private sealed trait TestEvent extends NoKeyEvent
  private case class AddEvent(string: String) extends TestEvent
  private case object InvalidEvent extends TestEvent

  private case class TestState(string: String)
  extends EventDrivenState[TestState, TestEvent]
  {
    def applyEvent(keyedEvent: KeyedEvent[TestEvent]) =
      keyedEvent match {
        case KeyedEvent(NoKey, AddEvent(s)) =>
          Right(copy(string = string + s))
        case _ =>
          eventNotApplicable(keyedEvent)
      }
  }
}
