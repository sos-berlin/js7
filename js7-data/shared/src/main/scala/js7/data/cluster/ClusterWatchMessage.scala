package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.GenericLong
import js7.base.log.CorrelId
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchMessage.*
import js7.data.node.NodeId

sealed trait ClusterWatchMessage
{
  def correlId: CorrelId
  def from: NodeId
  def clusterState: HasNodes
}

sealed trait ClusterWatchRequest extends ClusterWatchMessage
{
  def requestId: RequestId

  def toShortString: String
}

final case class ClusterWatchCheckEvent(
  requestId: RequestId,
  correlId: CorrelId,
  from: NodeId,
  event: ClusterEvent,
  clusterState: ClusterState.HasNodes,
  checkOnly: Boolean = false /*???*/)
extends ClusterWatchRequest
{
  override def toShortString =
    s"$requestId ${event.getClass.simpleScalaName} event"
}

final case class ClusterWatchCheckState(
  requestId: RequestId,
  correlId: CorrelId,
  from: NodeId,
  clusterState: ClusterState.HasNodes)
extends ClusterWatchRequest
{
  override def toShortString =
    s"$requestId ${clusterState.toShortString} state"
}

object ClusterWatchMessage
{
  final case class RequestId(number: Long) extends GenericLong {
    def increment = RequestId(number + 1)
    override def toString = s"Request:$number"
  }
  object RequestId extends GenericLong.Companion[RequestId]

  implicit val jsonCodec: TypedJsonCodec[ClusterWatchMessage] = TypedJsonCodec(
    Subtype(deriveCodec[ClusterWatchCheckState]),
    Subtype(deriveCodec[ClusterWatchCheckEvent]))
}
