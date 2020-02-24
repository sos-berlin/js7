package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import scala.collection.immutable.Seq

sealed trait ClusterWatchMessage

final case class ClusterWatchEvents(
  from: Uri,
  events: Seq[ClusterEvent],
  clusterState: ClusterState)
extends ClusterWatchMessage

final case class ClusterWatchHeartbeat(from: Uri, clusterState: ClusterState)
extends ClusterWatchMessage

object ClusterWatchMessage
{
  implicit val jsonCodec = TypedJsonCodec[ClusterWatchMessage](
    Subtype(deriveCodec[ClusterWatchEvents]),
    Subtype(deriveCodec[ClusterWatchHeartbeat]))
}
