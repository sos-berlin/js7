package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.IntelliJUtils.intelliJuseImport

/** Command from ClusterWatch to a cluster node. */
sealed trait ClusterWatchingCommand

object ClusterWatchingCommand
{
  // Command from ClusterWatch
  final case class ClusterWatchConfirm(
    requestId: ClusterWatchRequest.RequestId,
    clusterWatchId: ClusterWatchId,
    clusterWatchRunId: ClusterWatchRunId,
    manualConfirmer: Option[String],
    problem: Option[Problem])
  extends ClusterWatchingCommand
  {
    override def toString =
      s"ClusterWatchConfirm($argString)"

    def argString =
      s"$requestId $clusterWatchId $clusterWatchRunId${problem.fold("")(o => s" ⛔ $o")}"
  }

  implicit val jsonCodec: TypedJsonCodec[ClusterWatchingCommand] = TypedJsonCodec(
    Subtype(deriveCodec[ClusterWatchConfirm]))

  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))
}
