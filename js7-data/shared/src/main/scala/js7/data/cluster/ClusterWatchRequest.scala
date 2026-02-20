package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.GenericLong
import js7.base.log.CorrelId
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.cluster.ClusterEvent.{ClusterNodeLostEvent, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchProblems.InvalidClusterWatchHeartbeatProblem
import js7.data.cluster.ClusterWatchRequest.*
import js7.data.node.NodeId
import scala.concurrent.duration.FiniteDuration

sealed trait ClusterWatchRequest:
  def checked: Checked[this.type]
  def requestId: RequestId
  def correlId: CorrelId
  def from: NodeId
  def maybeEvent: Option[ClusterEvent]
  def clusterState: HasNodes
  def forceWhenUntaught: Boolean
  def toShortString: String

  def isNodeLostEvent(lostNodeId: NodeId): Boolean =
    maybeEvent match
      case Some(event: ClusterNodeLostEvent) => event.lostNodeId == lostNodeId
      case _ => false


sealed trait ClusterWatchNonCommitRequest extends ClusterWatchRequest


sealed trait ClusterWatchEventRequest extends ClusterWatchRequest:
  def event: ClusterEvent

  def checked: Checked[this.type] =
    if from != clusterState.activeId && !event.isInstanceOf[ClusterSwitchedOver] then
      Left(InvalidClusterWatchHeartbeatProblem(from, clusterState))
    else
      Right(this)


final case class ClusterWatchCheckEvent(
  requestId: RequestId,
  correlId: CorrelId,
  from: NodeId,
  event: ClusterEvent,
  clusterState: ClusterState.HasNodes,
  forceWhenUntaught: Boolean = false)
extends ClusterWatchEventRequest, ClusterWatchNonCommitRequest:

  def maybeEvent: Some[ClusterEvent] =
    Some(event)

  override def toShortString =
    s"$requestId:Event:${event.getClass.simpleScalaName}"


/**
  * @param askNodeLoss If Some(duration), hold the breath for the given duration until
  *                    a committing `ClusterWatchAskNodeLoss`.
  *                    If None, commit the `ClusterNodeLostEvent`.
  */
final case class ClusterWatchAskNodeLoss(
  requestId: RequestId,
  correlId: CorrelId,
  from: NodeId,
  event: ClusterNodeLostEvent,
  clusterState: ClusterState.IsNodeLost,
  hold: FiniteDuration)
extends ClusterWatchEventRequest, ClusterWatchNonCommitRequest:

  def forceWhenUntaught = false

  def maybeEvent: Some[ClusterNodeLostEvent] =
    Some(event)

  override def toShortString =
    s"$requestId:ClusterWatchAskNodeLoss:${event.getClass.simpleScalaName}"


final case class ClusterWatchCommitNodeLoss(
  requestId: RequestId,
  correlId: CorrelId,
  from: NodeId,
  event: ClusterNodeLostEvent,
  clusterState: ClusterState.IsNodeLost)
extends ClusterWatchEventRequest:

  def forceWhenUntaught = false

  def maybeEvent: Some[ClusterNodeLostEvent] =
    Some(event)

  override def toShortString =
    s"$requestId:ClusterWatchCommitNodeLoss:${event.getClass.simpleScalaName}"


final case class ClusterWatchCheckState(
  requestId: RequestId,
  correlId: CorrelId,
  from: NodeId,
  clusterState: ClusterState.HasNodes)
extends ClusterWatchNonCommitRequest:
  def checked: Checked[this.type] =
    if from != clusterState.activeId then
      Left(InvalidClusterWatchHeartbeatProblem(from, clusterState))
    else
      Right(this)

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
    Subtype(deriveCodec[ClusterWatchCheckEvent]),
    Subtype(deriveCodec[ClusterWatchAskNodeLoss]),
    Subtype(deriveCodec[ClusterWatchCommitNodeLoss]))
