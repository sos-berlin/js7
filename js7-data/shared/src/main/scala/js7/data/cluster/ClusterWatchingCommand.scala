package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.IntelliJUtils.intelliJuseImport

/** Command from ClusterWatch to a cluster node. */
sealed trait ClusterWatchingCommand:
  def toShortString: String = toString


object ClusterWatchingCommand:

  /** Confirm or reject a node loss.
    *
    * A command from ClusterWatch to a cluster node (ClusterWatchCounterpart).
    *
    * When the ClusterWatch is requested to acknowledge a ClusterNodeLostEvent
    * but cannot determine whether a node is lost (down),
    * then the confirmation of an external subject (a person) is required to continue.
    *
    * @param manualConfirmer filled if an external subject (a person) has confirmed the node loss
    * @param problem if the node loss is not confirmed but rejected.
    */
  final case class ClusterWatchConfirm(
    requestId: ClusterWatchRequest.RequestId,
    clusterWatchId: ClusterWatchId,
    clusterWatchRunId: ClusterWatchRunId,
    manualConfirmer: Option[Confirmer],
    problem: Option[Problem])
  extends ClusterWatchingCommand:
    override def toString =
      s"ClusterWatchConfirm($argString)"

    def argString =
      s"$requestId $clusterWatchId $clusterWatchRunId${problem.fold("")(o => s" ⛔ $o")}"

  implicit val jsonCodec: TypedJsonCodec[ClusterWatchingCommand] = TypedJsonCodec(
    Subtype(deriveCodec[ClusterWatchConfirm]))

  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))
