package js7.cluster.watch.api

import js7.base.auth.UserId
import js7.base.problem.Problem
import js7.data.cluster.{ClusterWatchId, ClusterWatchRequest, ClusterWatchRunId}

sealed trait ClusterWatchConfirmation
{
  def requestId: ClusterWatchRequest.RequestId
  def confirmer: String
  def problem: Option[Problem]
}

final case class ConfirmedByClusterWatch(
  requestId: ClusterWatchRequest.RequestId,
  clusterWatchId: ClusterWatchId,
  clusterWatchRunId: ClusterWatchRunId,
  problem: Option[Problem])
extends ClusterWatchConfirmation
{
  def confirmer = clusterWatchId.toString
  override def toString =
    s"ConfirmedByClusterWatch($requestId $clusterWatchId $clusterWatchRunId${
      problem.fold("")(o => s"🚫 $o")})"
}

/** The ClusterWatchRequest has been confirmed by a Users inplace of a ClusterWatch. */
final case class ConfirmedByUser(
  requestId: ClusterWatchRequest.RequestId,
  userId: UserId)
extends ClusterWatchConfirmation
{
  def confirmer = userId.toString

  def problem = None

  override def toString = s"ConfirmedByUser($requestId $userId)"
}
