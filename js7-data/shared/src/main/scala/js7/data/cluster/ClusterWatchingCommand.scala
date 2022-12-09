package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem

/** Command from ClusterWatch to a cluster node. */
sealed trait ClusterWatchingCommand

object ClusterWatchingCommand
{
  // Command from ClusterWatch
  final case class ClusterWatchAcknowledge(
    requestId: ClusterWatchMessage.RequestId,
    problem: Option[Problem])
  extends ClusterWatchingCommand

  implicit val jsonCodec: TypedJsonCodec[ClusterWatchingCommand] = TypedJsonCodec(
    Subtype(deriveCodec[ClusterWatchAcknowledge]))
}
