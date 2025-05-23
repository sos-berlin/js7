package js7.data.event

import js7.data.cluster.{ClusterState, ClusterStateSnapshot}

trait StandardsRecoverer:
  this: SnapshotableStateRecoverer[?] =>

  private var _standards: SnapshotableState.Standards =
    SnapshotableState.Standards.empty

  protected final def standards: SnapshotableState.Standards =
    _standards

  def journalState: JournalState =
    _standards.journalState

  def clusterState: ClusterState =
    _standards.clusterState

  protected final def addStandardObject(obj: JournalState | ClusterStateSnapshot): Unit =
    obj match
      case o: JournalState =>
        _standards = _standards.copy(journalState = o)

      case ClusterStateSnapshot(o) =>
        _standards = _standards.copy(clusterState = o)
