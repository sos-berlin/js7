package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
import monix.eval.Task
import scala.collection.immutable.Seq

trait ClusterWatchApi
{
  def get: Task[Checked[ClusterState]]

  def applyEvents(from: ClusterNodeId, events: Seq[ClusterEvent], reportedClusterState: ClusterState, force: Boolean = false)
  : Task[Checked[Completed]]

  def heartbeat(from: ClusterNodeId, reportedClusterState: ClusterState): Task[Checked[Completed]]
}
