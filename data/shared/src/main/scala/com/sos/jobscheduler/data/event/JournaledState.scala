package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.event.JournaledState._
import monix.reactive.Observable
import scala.collection.immutable._

trait JournaledState[Self <: JournaledState[Self, E], E <: Event]
{
  def applyEvent(keyedEvent: KeyedEvent[E]): Checked[Self]

  def withEventId(eventId: EventId): Self

  def toSnapshotObservable: Observable[Any]

  final def applyEvents(keyedEvents: Seq[KeyedEvent[E]]): Checked[Self] = {
    var state = this.asInstanceOf[Self]
    var problem: Problem = null
    for (keyedEvent <- keyedEvents if problem == null) {
      state.applyEvent(keyedEvent) match {
        case Left(o) => problem = o
        case Right(s) => state = s
      }
    }
    if (problem != null) Left(problem)
    else Right(state)
  }

  protected final def eventNotApplicable(keyedEvent: KeyedEvent[E]) =
    Left(EventNotApplicableProblem(this, keyedEvent))
}

object JournaledState {

  final case class EventNotApplicableProblem(state: Any, event: KeyedEvent[Event]) extends Problem.Coded {
    def arguments = Map(
      "state" -> state.toString.truncateWithEllipsis(100),
      "event" -> event.toString.truncateWithEllipsis(100))
  }
}
