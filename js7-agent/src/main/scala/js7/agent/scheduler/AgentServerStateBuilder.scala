package js7.agent.scheduler

import js7.base.problem.Checked._
import js7.data.cluster.ClusterState
import js7.data.event.{JournalState, JournaledStateBuilder}

final class AgentServerStateBuilder
extends JournaledStateBuilder[AgentServerState]
{
  private var _state = AgentServerState.empty

  protected def onInitializeState(state: AgentServerState) =
    _state = state

  protected def onAddSnapshot = {
    case snapshot: RegisteredController =>
      _state = state.applySnapshot(snapshot).orThrow
  }

  protected def onOnAllSnapshotsAdded() = {}

  protected def onAddEvent = {
    case stamped => _state = state.applyStampedEvents(stamped :: Nil).orThrow
  }

  def state = _state

  def journalState = JournalState.empty

  def clusterState = ClusterState.Empty
}
