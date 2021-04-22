package js7.agent.scheduler

import java.util.UUID
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerId
import js7.data.event.{EventId, JournalId, JournalState, JournaledState}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentServerStateTest extends AsyncFreeSpec
{
  private lazy val agentServerState = AgentServerState(
    EventId(1000L),
    JournaledState.Standards(
      JournalState(Map.empty),
      ClusterState.Empty),
    Map(
      ControllerId("CONTROLLER") -> RegisteredController(
        ControllerId("CONTROLLER"),
        AgentPath("AGENT"),
        AgentRunId(JournalId(UUID.fromString("11111111-2222-3333-4444-555555555555"))))))

  "estimatedSnapshotSize" in {
    assert(agentServerState.estimatedSnapshotSize == 1)
    for (n <- agentServerState.toSnapshotObservable.countL.runToFuture)
      yield assert(n == agentServerState.estimatedSnapshotSize)
  }

  "toSnapshotObservable" in {
    for (list <- agentServerState.toSnapshotObservable.toListL.runToFuture)
      yield assert(list == agentServerState.idToController.values.toList)
  }
}
