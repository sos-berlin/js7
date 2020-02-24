package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import monix.eval.Task
import scala.collection.immutable.Seq

trait ClusterWatchApi
{
  def get: Task[Checked[ClusterState]]

  def applyEvents(from: Uri, events: Seq[ClusterEvent], reportedClusterState: ClusterState): Task[Checked[Completed]]

  def heartbeat(from: Uri, reportedClusterState: ClusterState): Task[Checked[Completed]]
}
