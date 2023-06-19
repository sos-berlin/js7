package js7.data.event

import js7.data.cluster.{ClusterState, ClusterStateSnapshot}

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
}
