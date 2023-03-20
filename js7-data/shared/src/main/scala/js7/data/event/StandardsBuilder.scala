package js7.data.event

import js7.base.problem.Checked.*
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterStateSnapshot}
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey

trait StandardsBuilder
{
  this: SnapshotableStateBuilder[?] =>

  protected var _standards: SnapshotableState.Standards =
    SnapshotableState.Standards.empty

  def journalState: JournalState =
    _standards.journalState

  def clusterState: ClusterState =
    _standards.clusterState

  protected final def addStandardObject(obj: Any): Unit =
    obj match {
      case o: JournalState =>
        _standards = _standards.copy(journalState = o)

      case ClusterStateSnapshot(o) =>
        _standards = _standards.copy(clusterState = o)
    }

  protected final def onStandardEvent(stamped: Stamped[KeyedEvent[Event]]): Unit = {
    stamped match {
      case Stamped(eventId, _, KeyedEvent(_: NoKey, event: JournalEvent)) =>
        updateEventId(eventId)
        _standards = _standards.copy(
          journalState = _standards.journalState.applyEvent(event))

      case Stamped(eventId, _, KeyedEvent(_: NoKey, event: ClusterEvent)) =>
        updateEventId(eventId)
        _standards = _standards.copy(
          clusterState = _standards.clusterState.applyEvent(event).orThrow)

      case _ => Left(EventNotApplicableProblem(stamped.value, toString))
    }
  }
}
