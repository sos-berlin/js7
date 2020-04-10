package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
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
