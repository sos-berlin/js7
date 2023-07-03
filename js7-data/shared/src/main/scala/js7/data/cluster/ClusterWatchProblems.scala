package js7.data.cluster

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.Problem
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.ClusterNodeLostEvent
import js7.data.node.NodeId
import scala.concurrent.duration.FiniteDuration

object ClusterWatchProblems
{
  //private lazy val isClusterWatchProblemCode = Set[ProblemCode](
  //  UntaughtClusterWatchProblem.code,
  //  ClusterWatchEventMismatchProblem.code,
  //  ClusterWatchInactiveNodeProblem.code,
  //  InvalidClusterWatchHeartbeatProblem.code,  // <-- this is in js7-data/ClusterWatchRequest
  //  ClusterFailOverWhilePassiveLostProblem.code)
  //
  //def isClusterWatchProblem(problem: Problem): Boolean =
  //  problem.maybeCode exists isClusterWatchProblemCode

  final case object UntaughtClusterWatchProblem extends Problem.ArgumentlessCoded

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

  final case object ClusterFailOverWhilePassiveLostProblem extends Problem.ArgumentlessCoded

  final case class ClusterNodeLossNotConfirmedProblem(
    fromNodeId: NodeId,
    event: ClusterNodeLostEvent)
    extends Problem.Coded {
    def arguments = Map(
      "fromNodeId" -> fromNodeId.toString,
      "event" -> event.toString)
  }
  object ClusterNodeLossNotConfirmedProblem extends Problem.Coded.Companion {
    implicit val jsonCodec: Codec.AsObject[ClusterNodeLossNotConfirmedProblem] =
      deriveCodec[ClusterNodeLossNotConfirmedProblem]
  }

  final case class ClusterNodeIsNotLostProblem(nodeId: NodeId) extends Problem.Coded {
    def arguments = Map(
      "nodeId" -> nodeId.toString)
  }

  case object ClusterWatchRequestDoesNotMatchProblem extends Problem.ArgumentlessCoded

  case object NoClusterWatchProblem extends Problem.ArgumentlessCoded

  final case class ClusterWatchIdDoesNotMatchProblem(
    rejectedClusterWatchId: ClusterWatchId,
    requestedClusterWatchId: ClusterWatchId)
  extends Problem.Coded {
    def arguments = Map(
      "rejectedClusterWatchId" -> rejectedClusterWatchId.toString,
      "requestedClusterWatchId" -> requestedClusterWatchId.toString,
    )
  }

  final case class OtherClusterWatchStillAliveProblem(
    rejectedClusterWatchId: ClusterWatchId,
    requestedClusterWatchId: ClusterWatchId)
  extends Problem.Coded {
    def arguments = Map(
      "rejectedClusterWatchId" -> rejectedClusterWatchId.toString,
      "requestedClusterWatchId" -> requestedClusterWatchId.toString,
    )
  }

  final case class ClusterWatchIdNotUniqueProblem(
    clusterWatchId: ClusterWatchId,
    clusterWatchRunId: ClusterWatchRunId)
  extends Problem.Coded {
    def arguments = Map(
      "clusterWatchId" -> clusterWatchId.toString,
      "clusterWatchRunId" -> clusterWatchRunId.toString)
  }

  case object ClusterStateEmptyProblem extends Problem.ArgumentlessCoded
}
