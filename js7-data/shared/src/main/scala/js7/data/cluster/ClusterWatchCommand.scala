package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem

/** Command from ClusterWatch to a cluster node. */
sealed trait ClusterWatchCommand

object ClusterWatchCommand
{
  // Command from ClusterWatch
  final case class ClusterWatchAcknowledge(
    requestId: ClusterWatchMessage.RequestId,
    problem: Option[Problem])
  extends ClusterWatchCommand

  implicit val jsonCodec: TypedJsonCodec[ClusterWatchCommand] = TypedJsonCodec(
    Subtype(deriveCodec[ClusterWatchAcknowledge]))
}
