package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import com.sos.jobscheduler.data.event.JournaledState._
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import monix.reactive.Observable

trait JournaledState[Self <: JournaledState[Self, E], E <: Event]
{
  self: Self =>

  def toSnapshotObservable: Observable[Any]

  protected def standards: Standards

  protected def withStandards(standards: Standards): Self

  final def journalState: JournalState =
    standards.journalState

  final def clusterState: ClusterState =
    standards.clusterState

  def applyEvent(keyedEvent: KeyedEvent[E]): Checked[Self]

  def withEventId(eventId: EventId): Self

  final def applyEvents(keyedEvents: Seq[KeyedEvent[E]]): Checked[Self] = {
    var state = self
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

  protected final def applyStandardEvent(keyedEvent: KeyedEvent[E]): Checked[Self] =
    keyedEvent match {
      case KeyedEvent(_: NoKey, _: SnapshotTaken) =>
        Right(self)

      case KeyedEvent(_: NoKey, event: JournalEventsReleased) =>
        Right(withStandards(standards.copy(
          journalState = journalState.applyEvent(event))))

      case KeyedEvent(_: ClusterEvent#Key, _: ClusterEvent) =>
        for (o <- clusterState.applyEvent(keyedEvent.asInstanceOf[KeyedEvent[ClusterEvent]]))
          yield withStandards(standards.copy(
            clusterState = o))

      case _ => eventNotApplicable(keyedEvent)
    }

  protected final def eventNotApplicable(keyedEvent: KeyedEvent[E]) =
    Left(EventNotApplicableProblem(keyedEvent, this))
}

object JournaledState
{
  final case class Standards(journalState: JournalState, clusterState: ClusterState)
  {
    def toSnapshotObservable: Observable[Any] =
      journalState.toSnapshotObservable ++
        clusterState.toSnapshotObservable
  }
  object Standards
  {
    def empty = Standards(JournalState.empty, ClusterState.Empty)
  }

  final case class EventNotApplicableProblem(keyedEvent: KeyedEvent[Event], state: Any) extends Problem.Coded {
    def arguments = Map(
      "event" -> keyedEvent.toString.truncateWithEllipsis(100),
      "state" -> state.toString.truncateWithEllipsis(100))
  }
}
