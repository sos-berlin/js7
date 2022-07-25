package js7.data.cluster

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.command.CommonCommand
import js7.data.event.EventId
import js7.data.node.NodeId
import scala.concurrent.duration.FiniteDuration

sealed trait ClusterCommand extends CommonCommand {
  type Response <: ClusterCommand.Response
}

object ClusterCommand
{
  final case class ClusterStartBackupNode(setting: ClusterSetting, fileEventId: EventId)
  extends ClusterCommand {
    type Response = Response.Accepted
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

  final case class ClusterPassiveDown(activeId: NodeId, passiveId: NodeId)
  extends ClusterCommand {
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterPassiveDown(activeId=$activeId passiveId=$passiveId)"
  }

  final case class ClusterInhibitActivation(duration: FiniteDuration)
  extends ClusterCommand {
    type Response = ClusterInhibitActivation.Response
  }
  object ClusterInhibitActivation {
    final case class Response(failedOver: Option[ClusterState.FailedOver])
    extends ClusterCommand.Response

    private implicit val x: Codec.AsObject[ClusterState.FailedOver] =
      deriveCodec[ClusterState.FailedOver]
    implicit val jsonCodec: Codec.AsObject[Response] = deriveCodec
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
    Subtype(deriveCodec[ClusterPassiveDown]),
    Subtype(deriveCodec[ClusterInhibitActivation]))

  intelliJuseImport(FiniteDurationJsonEncoder)
}
