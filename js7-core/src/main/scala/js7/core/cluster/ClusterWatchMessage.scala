package js7.core.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.node.NodeId

sealed trait ClusterWatchMessage

final case class ClusterWatchEvents(
  from: NodeId,
  events: Seq[ClusterEvent],
  clusterState: ClusterState,
  checkOnly: Boolean = false)
extends ClusterWatchMessage

final case class ClusterWatchHeartbeat(from: NodeId, clusterState: ClusterState)
extends ClusterWatchMessage

object ClusterWatchMessage
{
  implicit val jsonCodec: TypedJsonCodec[ClusterWatchMessage] = TypedJsonCodec(
    Subtype(deriveCodec[ClusterWatchEvents]),
    Subtype(deriveCodec[ClusterWatchHeartbeat]))

  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))
}
