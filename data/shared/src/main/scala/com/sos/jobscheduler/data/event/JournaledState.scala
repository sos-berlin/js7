package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.JournaledState._
import monix.reactive.Observable

trait JournaledState[Self <: JournaledState[Self, E], E <: Event]
{
  def toSnapshotObservable: Observable[Any]

  def journalState: JournalState

  def clusterState: ClusterState

  def applyEvent(keyedEvent: KeyedEvent[E]): Checked[Self]

  def withEventId(eventId: EventId): Self

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
    Left(EventNotApplicableProblem(keyedEvent, this))
}

object JournaledState {

  final case class EventNotApplicableProblem(keyedEvent: KeyedEvent[Event], state: Any) extends Problem.Coded {
    def arguments = Map(
      "event" -> keyedEvent.toString.truncateWithEllipsis(100),
      "state" -> state.toString.truncateWithEllipsis(100))
  }
}
