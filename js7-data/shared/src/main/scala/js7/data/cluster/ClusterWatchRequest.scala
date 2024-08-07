package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.GenericLong
import js7.base.log.CorrelId
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.cluster.ClusterEvent.{ClusterNodeLostEvent, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchRequest.*
import js7.data.node.NodeId

sealed trait ClusterWatchRequest:
  def checked: Checked[this.type]
  def requestId: RequestId
  def correlId: CorrelId
  def from: NodeId
  def clusterState: HasNodes
  def maybeEvent: Option[ClusterEvent]
  def isNodeLostEvent(lostNodeId: NodeId): Boolean
  def forceWhenUntaught: Boolean
  def toShortString: String

final case class ClusterWatchCheckEvent(
  requestId: RequestId,
  correlId: CorrelId,
  from: NodeId,
  event: ClusterEvent,
  clusterState: ClusterState.HasNodes,
  forceWhenUntaught: Boolean = false)
extends ClusterWatchRequest:
  def checked: Checked[this.type] =
    if from != clusterState.activeId && !event.isInstanceOf[ClusterSwitchedOver] then
      Left(InvalidClusterWatchHeartbeatProblem(from, clusterState))
    else
      Right(this)

  def isNodeLostEvent(lostNodeId: NodeId): Boolean =
    event match
      case event: ClusterNodeLostEvent => event.lostNodeId == lostNodeId
      case _ => false

  def maybeEvent: Some[ClusterEvent] =
    Some(event)

  override def toShortString =
    s"$requestId:Event:${event.getClass.simpleScalaName}"

final case class ClusterWatchCheckState(
  requestId: RequestId,
  correlId: CorrelId,
  from: NodeId,
  clusterState: ClusterState.HasNodes)
extends ClusterWatchRequest:
  def checked: Checked[this.type] =
    if from != clusterState.activeId then
      Left(InvalidClusterWatchHeartbeatProblem(from, clusterState))
    else
      Right(this)

  def isNodeLostEvent(lostNodeId: NodeId) = false

  def maybeEvent: None.type =
    None

  def forceWhenUntaught = false

  override def toShortString =
    s"$requestId:ClusterState:${clusterState.toShortString}"


object ClusterWatchRequest:
  final case class RequestId(number: Long) extends GenericLong:
    def increment: RequestId = RequestId(number + 1)
    override def toString = s"Request:$number"
  object RequestId extends GenericLong.Companion[RequestId]

  implicit val jsonCodec: TypedJsonCodec[ClusterWatchRequest] = TypedJsonCodec(
    Subtype(deriveCodec[ClusterWatchCheckState]),
    Subtype(deriveCodec[ClusterWatchCheckEvent]))

final case class InvalidClusterWatchHeartbeatProblem(from: NodeId, clusterState: ClusterState)
  extends Problem.Coded:

  def arguments: Map[String, String] = Map(
    "from" -> from.string,
    "clusterState" -> clusterState.toString)

// TODO Move InvalidClusterWatchHeartbeatProblem to js7-cluster-watch-api


object InvalidClusterWatchHeartbeatProblem extends Problem.Coded.Companion
