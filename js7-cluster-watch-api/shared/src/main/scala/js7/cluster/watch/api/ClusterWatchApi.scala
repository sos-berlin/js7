package js7.cluster.watch.api

import js7.base.problem.Checked
import js7.data.cluster.ClusterEvent
import js7.data.cluster.ClusterState.HasNodes
import monix.eval.Task

trait ClusterWatchApi
{
  def stop: Task[Unit]

  def checkClusterState(clusterState: HasNodes, clusterWatchIdChangeAllowed: Boolean)
  : Task[Checked[Option[ClusterWatchConfirmation]]]

  def applyEvent(event: ClusterEvent, clusterState: HasNodes)
  : Task[Checked[Option[ClusterWatchConfirmation]]]
}
