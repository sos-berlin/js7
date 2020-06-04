package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.cluster.ClusterSetting.syntax._
import com.sos.jobscheduler.data.command.CommonCommand
import com.sos.jobscheduler.data.event.EventId
import scala.concurrent.duration.FiniteDuration

sealed trait ClusterCommand extends CommonCommand {
  type Response <: ClusterCommand.Response
}

object ClusterCommand
{
  final case class ClusterStartBackupNode(idToUri: Map[ClusterNodeId, Uri], activeId: ClusterNodeId, fileEventId: EventId)
  extends ClusterCommand {
    type Response = Response.Accepted
    ClusterSetting.checkUris(idToUri, activeId).orThrow

    def passiveId: ClusterNodeId =
      idToUri.peerOf(activeId)
  }

  final case class ClusterPrepareCoupling(activeId: ClusterNodeId, passiveId: ClusterNodeId)
  extends ClusterCommand {
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterPrepareCoupling(activeId=$activeId passiveId=$passiveId)"
  }

  final case class ClusterCouple(activeId: ClusterNodeId, passiveId: ClusterNodeId)
  extends ClusterCommand {
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterCouple(activeId=$activeId passiveId=$passiveId)"
  }

  final case class ClusterRecouple(activeId: ClusterNodeId, passiveId: ClusterNodeId)
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
