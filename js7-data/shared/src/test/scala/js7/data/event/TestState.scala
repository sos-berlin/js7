package js7.data.event

import js7.data.event.KeyedEvent.NoKey
import js7.data.event.TestEvent.Added

final case class TestState(string: String)
extends EventDrivenState[TestState, TestEvent]:

  def companion = TestCase

  def applyKeyedEvent(keyedEvent: KeyedEvent[TestEvent]) =
    keyedEvent match
      case KeyedEvent(NoKey, Added(s)) =>
        Right(copy(string = string + s))
      case _ =>
        eventNotApplicable(keyedEvent)


object TestCase extends EventDrivenState.Companion[TestState, TestEvent]

sealed trait TestEvent extends NoKeyEvent

object TestEvent:
  final case class Added(string: String) extends TestEvent

  case object InvalidEvent extends TestEvent
