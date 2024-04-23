package js7.cluster.watch.api

import js7.data.cluster.{ClusterWatchId, ClusterWatchRequest, ClusterWatchRunId}

final case class ClusterWatchConfirmation(
  requestId: ClusterWatchRequest.RequestId,
  clusterWatchId: ClusterWatchId,
  clusterWatchRunId: ClusterWatchRunId):

  def confirmer: String =
    clusterWatchId.toString

  override def toString =
    s"ConfirmedByClusterWatch($requestId $clusterWatchId $clusterWatchRunId)"
