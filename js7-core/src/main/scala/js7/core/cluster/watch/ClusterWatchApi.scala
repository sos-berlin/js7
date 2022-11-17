package js7.core.cluster.watch

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.cluster.ClusterState
import js7.data.node.NodeId
import monix.eval.Task

trait ClusterWatchApi
{
  // HttpSessionApi may implement this method
  def logout(): Task[Completed]

  def applyEvents(clusterWatchEvents: ClusterWatchEvents): Task[Checked[Completed]]

  def heartbeat(from: NodeId, reportedClusterState: ClusterState): Task[Checked[Completed]]
}
