package js7.cluster.watch.api

import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.data.cluster.ClusterEvent
import js7.data.cluster.ClusterState.HasNodes
import monix.eval.Task

trait AnyClusterWatch
{
  def tryLogout: Task[Completed]

  def checkClusterState(clusterState: HasNodes): Task[Checked[Completed]]

  def heartbeat(clusterState: HasNodes): Task[Checked[Completed]]

  def applyEvent(event: ClusterEvent, clusterState: HasNodes): Task[Checked[Completed]]
}