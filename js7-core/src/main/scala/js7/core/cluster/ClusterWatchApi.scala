package js7.core.cluster

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.node.NodeId
import monix.eval.Task
trait ClusterWatchApi
{
  def get: Task[Checked[ClusterState]]

  def applyEvents(from: NodeId, events: Seq[ClusterEvent], reportedClusterState: ClusterState, force: Boolean = false)
  : Task[Checked[Completed]]

  def heartbeat(from: NodeId, reportedClusterState: ClusterState): Task[Checked[Completed]]
}
