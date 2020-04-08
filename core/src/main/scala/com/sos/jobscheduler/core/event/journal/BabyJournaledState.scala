package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, JournalState, JournaledState, KeyedEvent}

// This class is meant to grow up to a big JournaledState
// containing the whole state (MasterState, AgentState) stored in the journal (the snapshot).
// It may be keept by JournalActor for quick and safe snapshot access.
final case class BabyJournaledState(
  eventId: EventId,
  journalState: JournalState,
  clusterState: ClusterState = ClusterState.Empty)
extends JournaledState[BabyJournaledState, Event]
{
  def toSnapshotObservable =
    journalState.toSnapshotObservable ++
      clusterState.toSnapshotObservable

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match {
      case KeyedEvent(_: NoKey, SnapshotTaken) =>
        Right(this)

      case KeyedEvent(_: NoKey, event: JournalEventsReleased) =>
        Right(copy(
          journalState = journalState.applyEvent(event)))

      case KeyedEvent(_: ClusterEvent#Key, _: ClusterEvent) =>
        for (o <- clusterState.applyEvent(keyedEvent.asInstanceOf[KeyedEvent[ClusterEvent]]))
          yield copy(clusterState = o)

      case _ => eventNotApplicable(keyedEvent)
    }

  def withEventId(eventId: EventId) = throw new NotImplementedError
}

object BabyJournaledState
{
  val empty = BabyJournaledState(EventId.BeforeFirst, JournalState.empty, ClusterState.Empty)
}
