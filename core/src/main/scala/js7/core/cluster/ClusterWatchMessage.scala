package js7.core.cluster

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
sealed trait ClusterWatchMessage

final case class ClusterWatchEvents(
  from: ClusterNodeId,
  events: Seq[ClusterEvent],
  clusterState: ClusterState,
  force: Boolean = false)
extends ClusterWatchMessage

final case class ClusterWatchHeartbeat(from: ClusterNodeId, clusterState: ClusterState)
extends ClusterWatchMessage

object ClusterWatchMessage
{
  implicit val jsonCodec = TypedJsonCodec[ClusterWatchMessage](
    Subtype(deriveCodec[ClusterWatchEvents]),
    Subtype(deriveCodec[ClusterWatchHeartbeat]))
}
