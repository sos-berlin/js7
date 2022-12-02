package js7.cluster.watch.api

import js7.base.problem.Problem
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.ClusterNodeLostEvent
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.node.NodeId
import scala.concurrent.duration.FiniteDuration

object ClusterWatchProblems
{
  final case object UntaughtClusterWatchProblem extends Problem.ArgumentlessCoded

  final case class ClusterWatchHeartbeatMismatchProblem(
    currentClusterState: ClusterState,
    reportedClusterState: ClusterState)
    extends Problem.Coded
  {
    //"Controller's ClusterState $currentClusterState does not match registered $clusterState")
    def arguments = Map(
      "currentClusterState" -> currentClusterState.toString,
      "reportedClusterState" -> reportedClusterState.toString)
  }

  object ClusterWatchHeartbeatMismatchProblem extends Problem.Coded.Companion

  final case class ClusterWatchEventMismatchProblem(
    event: Option[ClusterEvent],
    currentClusterState: ClusterState,
    reportedClusterState: ClusterState)
    extends Problem.Coded
  {
    //"Controller's ClusterState $currentClusterState does not match registered $clusterState")
    def arguments = Map(
      "event" -> event.fold("None")(_.toString),
      "currentClusterState" -> currentClusterState.toString,
      "reportedClusterState" -> reportedClusterState.toString)
  }

  object ClusterWatchEventMismatchProblem extends Problem.Coded.Companion

  final case class ClusterWatchInactiveNodeProblem(from: NodeId, clusterState: ClusterState,
    lastHeartbeatDuration: FiniteDuration, operation: String)
    extends Problem.Coded
  {
    def arguments = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString,
      "lastHeartbeat" -> lastHeartbeatDuration.pretty,
      "operation" -> operation)
  }

  object ClusterWatchInactiveNodeProblem extends Problem.Coded.Companion

  final case class InvalidClusterWatchHeartbeatProblem(from: NodeId, clusterState: ClusterState)
  extends Problem.Coded
  {
    def arguments = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString)
  }

  object InvalidClusterWatchHeartbeatProblem extends Problem.Coded.Companion

  final case object ClusterFailOverWhilePassiveLostProblem extends Problem.ArgumentlessCoded

  final case class ClusterNodeLossNotAcknowledgedProblem(event: ClusterNodeLostEvent)
  extends Problem.Coded
  {
    def arguments = Map(
      "event" -> event.toString)
  }
  object ClusterNodeLossNotAcknowledgedProblem extends Problem.Coded.Companion

  case object NoClusterNodeLostProblem extends Problem.ArgumentlessCoded

  case object NoClusterWatchRequestMatches extends Problem.ArgumentlessCoded
}
