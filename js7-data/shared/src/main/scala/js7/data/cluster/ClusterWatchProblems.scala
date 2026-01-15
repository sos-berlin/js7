package js7.data.cluster

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.ClusterNodeLostEvent
import js7.data.node.NodeId
import scala.collection.immutable.Map.{Map2, Map3}
import scala.concurrent.duration.FiniteDuration

object ClusterWatchProblems:
  //private lazy val isClusterWatchProblemCode = Set[ProblemCode](
  //  UntaughtClusterWatchProblem.code,
  //  ClusterWatchEventMismatchProblem.code,
  //  ClusterWatchInactiveNodeProblem.code,
  //  InvalidClusterWatchHeartbeatProblem.code,  // <-- this is in js7-data/ClusterWatchRequest
  //  ClusterFailOverWhilePassiveLostProblem.code)
  //
  //def isClusterWatchProblem(problem: Problem): Boolean =
  //  problem.maybeCode exists isClusterWatchProblemCode

  case object UntaughtClusterWatchProblem extends Problem.ArgumentlessCoded

  final case class ClusterWatchEventMismatchProblem(
    event: Option[ClusterEvent],
    currentClusterState: ClusterState,
    reportedClusterState: ClusterState)
    extends Problem.Coded:
    //"Controller's ClusterState $currentClusterState does not match registered $clusterState")
    def arguments: Map[String, String] = Map3(
      "event", event.fold("None")(_.toString),
      "currentClusterState", currentClusterState.toString,
      "reportedClusterState", reportedClusterState.toString)

  object ClusterWatchEventMismatchProblem extends Problem.Coded.Companion

  final case class ClusterWatchInactiveNodeProblem(from: NodeId, clusterState: ClusterState,
    lastHeartbeatDuration: FiniteDuration, operation: String)
    extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString,
      "lastHeartbeat" -> lastHeartbeatDuration.pretty,
      "operation" -> operation)

  object ClusterWatchInactiveNodeProblem extends Problem.Coded.Companion

  case object ClusterFailOverWhilePassiveLostProblem extends Problem.ArgumentlessCoded

  // Test only!
  type ClusterPassiveLostWhileFailedOverTestingProblem =
    ClusterPassiveLostWhileFailedOverTestingProblem.type
  case object ClusterPassiveLostWhileFailedOverTestingProblem extends Problem.ArgumentlessCoded

  final case class ClusterNodeLossNotConfirmedProblem(
    fromNodeId: NodeId,
    event: ClusterNodeLostEvent)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "fromNodeId", fromNodeId.toString,
      "event", event.toString)
  object ClusterNodeLossNotConfirmedProblem extends Problem.Coded.Companion:
    implicit val jsonCodec: Codec.AsObject[ClusterNodeLossNotConfirmedProblem] =
      TypedJsonCodec[ClusterNodeLossNotConfirmedProblem](
        Subtype(deriveCodec[ClusterNodeLossNotConfirmedProblem]))

  final case class ClusterNodeIsNotLostProblem(nodeId: NodeId, info: String)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "nodeId", nodeId.toString,
      "info", info)

  case object ClusterWatchRequestDoesNotMatchProblem extends Problem.ArgumentlessCoded

  case object NoClusterWatchProblem extends Problem.ArgumentlessCoded

  case object ClusterWatchNotAskingProblem extends Problem.ArgumentlessCoded

  final case class ClusterWatchIdDoesNotMatchProblem(
    rejectedClusterWatchId: ClusterWatchId,
    requestedClusterWatchId: ClusterWatchId,
    request: ClusterWatchRequest)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map3(
      "rejectedClusterWatchId", rejectedClusterWatchId.toString,
      "requestedClusterWatchId", requestedClusterWatchId.toString,
      "request", request.toShortString)

  final case class OtherClusterWatchStillAliveProblem(
    rejectedClusterWatchId: ClusterWatchId,
    requestedClusterWatchId: ClusterWatchId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "rejectedClusterWatchId", rejectedClusterWatchId.toString,
      "requestedClusterWatchId", requestedClusterWatchId.toString)

  final case class ClusterWatchIdNotUniqueProblem(
    clusterWatchId: ClusterWatchId,
    clusterWatchRunId: ClusterWatchRunId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "clusterWatchId", clusterWatchId.toString,
      "clusterWatchRunId", clusterWatchRunId.toString)


  type ClusterWatchActiveStillAliveProblem = ClusterWatchActiveStillAliveProblem.type
  object ClusterWatchActiveStillAliveProblem extends Problem.ArgumentlessCoded


  case object ClusterStateEmptyProblem extends Problem.ArgumentlessCoded


  final case class InvalidClusterWatchHeartbeatProblem(from: NodeId, clusterState: ClusterState)
    extends Problem.Coded:

    def arguments: Map[String, String] = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString)

  // TODO Move InvalidClusterWatchHeartbeatProblem to js7-cluster-watch-api
  object InvalidClusterWatchHeartbeatProblem extends Problem.Coded.Companion
