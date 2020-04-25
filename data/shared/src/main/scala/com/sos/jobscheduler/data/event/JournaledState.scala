package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import com.sos.jobscheduler.data.event.JournaledState._
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import monix.reactive.Observable

trait JournaledState[This <: JournaledState[This]]
extends EventDrivenState[This, Event]
{
  this: This =>

  def toSnapshotObservable: Observable[Any]

  protected def standards: Standards

  protected def withStandards(standards: Standards): This

  final def journalState: JournalState =
    standards.journalState

  final def clusterState: ClusterState =
    standards.clusterState

  override def applyStampedEvents(stampedEvents: Iterable[Stamped[KeyedEvent[Event]]]): Checked[This] =
    if (stampedEvents.isEmpty)
      Right(this)
    else
      super.applyStampedEvents(stampedEvents)
        .map(_.withEventId(stampedEvents.last.eventId))

  def applyEvent(keyedEvent: KeyedEvent[Event]): Checked[This]

  def withEventId(eventId: EventId): This

  protected final def applyStandardEvent(keyedEvent: KeyedEvent[Event]): Checked[This] =
    keyedEvent match {
      case KeyedEvent(_: NoKey, _: SnapshotTaken) =>
        Right(this)

      case KeyedEvent(_: NoKey, event: JournalEventsReleased) =>
        Right(withStandards(standards.copy(
          journalState = journalState.applyEvent(event))))

      case KeyedEvent(_: ClusterEvent#Key, _: ClusterEvent) =>
        for (o <- clusterState.applyEvent(keyedEvent.asInstanceOf[KeyedEvent[ClusterEvent]]))
          yield withStandards(standards.copy(
            clusterState = o))

      case _ => eventNotApplicable(keyedEvent)
    }

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
