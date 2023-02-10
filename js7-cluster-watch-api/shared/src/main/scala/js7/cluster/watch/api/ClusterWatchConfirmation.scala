package js7.cluster.watch.api

import js7.base.problem.Problem
import js7.data.cluster.{ClusterWatchId, ClusterWatchRequest, ClusterWatchRunId}

final case class ClusterWatchConfirmation(
  requestId: ClusterWatchRequest.RequestId,
  clusterWatchId: ClusterWatchId,
  clusterWatchRunId: ClusterWatchRunId,
  problem: Option[Problem])
{
  def confirmer = clusterWatchId.toString
  override def toString =
    s"ConfirmedByClusterWatch($requestId $clusterWatchId $clusterWatchRunId${
      problem.fold("")(o => s"🚫 $o")})"
}
