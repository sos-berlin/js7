package js7.data.cluster

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked.Ops
import js7.base.utils.Assertions.assertThat
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.web.Uri
import js7.data.cluster.ClusterSetting.syntax._
import js7.data.command.CommonCommand
import js7.data.event.EventId
import js7.data.node.NodeId
import scala.concurrent.duration.FiniteDuration

sealed trait ClusterCommand extends CommonCommand {
  type Response <: ClusterCommand.Response
}

object ClusterCommand
{
  final case class ClusterStartBackupNode(idToUri: Map[NodeId, Uri], activeId: NodeId, fileEventId: EventId)
  extends ClusterCommand {
    type Response = Response.Accepted
    ClusterSetting.checkUris(idToUri, activeId).orThrow

    def passiveId: NodeId =
      idToUri.peerOf(activeId)
  }

  final case class ClusterPrepareCoupling(activeId: NodeId, passiveId: NodeId)
  extends ClusterCommand {
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterPrepareCoupling(activeId=$activeId passiveId=$passiveId)"
  }

  final case class ClusterCouple(activeId: NodeId, passiveId: NodeId)
  extends ClusterCommand {
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterCouple(activeId=$activeId passiveId=$passiveId)"
  }

  final case class ClusterRecouple(activeId: NodeId, passiveId: NodeId)
  extends ClusterCommand {
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterRecouple(activeId=$activeId passiveId=$passiveId)"
  }

  final case class ClusterInhibitActivation(duration: FiniteDuration)
  extends ClusterCommand {
    type Response = ClusterInhibitActivation.Response
  }
  object ClusterInhibitActivation {
    final case class Response(failedOver: Option[ClusterState.FailedOver])
    extends ClusterCommand.Response

    private implicit val x = deriveCodec[ClusterState.FailedOver]
    implicit val jsonCodec = deriveCodec[Response]
  }

  sealed trait Response
  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted

    implicit val ResponseJsonCodec: TypedJsonCodec[Response] = TypedJsonCodec[Response](
      Subtype(Accepted),
      Subtype.named[ClusterInhibitActivation.Response]("ClusterInhibitActivation.Response"))
  }

  implicit val jsonCodec: TypedJsonCodec[ClusterCommand] = TypedJsonCodec[ClusterCommand](
    Subtype(deriveCodec[ClusterStartBackupNode]),
    Subtype(deriveCodec[ClusterPrepareCoupling]),
    Subtype(deriveCodec[ClusterCouple]),
    Subtype(deriveCodec[ClusterRecouple]),
    Subtype(deriveCodec[ClusterInhibitActivation]))

  intelliJuseImport(FiniteDurationJsonEncoder)
}
