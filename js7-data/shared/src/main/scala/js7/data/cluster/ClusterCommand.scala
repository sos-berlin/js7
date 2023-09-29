package js7.data.cluster

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.auth.UserId
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.OneTimeToken
import js7.data.command.CommonCommand
import js7.data.event.EventId
import js7.data.node.{NodeId, NodeName}
import scala.concurrent.duration.FiniteDuration

sealed trait ClusterCommand extends CommonCommand:
  type Response <: ClusterCommand.Response

object ClusterCommand:
  /** Initialize the backup node.
   * @param activeNodeName to look up password for login at the active node
   * @param passiveNodeUserId UserId to login at the active node
   */
  final case class ClusterStartBackupNode(
    setting: ClusterSetting,
    fileEventId: EventId,
    activeNodeName: NodeName,
    passiveNodeUserId: UserId)
  extends ClusterCommand:
    type Response = Response.Accepted

  sealed trait ClusterCouplingCommand extends ClusterCommand:
    def activeId: NodeId
    def passiveId: NodeId
    def token: OneTimeToken

  final case class ClusterPrepareCoupling(
    activeId: NodeId, passiveId: NodeId, token: OneTimeToken)
  extends ClusterCouplingCommand:
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterPrepareCoupling(activeId=$activeId passiveId=$passiveId)"

  final case class ClusterCouple(
    activeId: NodeId, passiveId: NodeId, token: OneTimeToken)
  extends ClusterCouplingCommand:
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterCouple(activeId=$activeId passiveId=$passiveId)"

  final case class ClusterRecouple(activeId: NodeId, passiveId: NodeId)
  extends ClusterCommand:
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterRecouple(activeId=$activeId passiveId=$passiveId)"

  final case class ClusterConfirmCoupling(token: OneTimeToken)
  extends ClusterCommand:
    type Response = Response.Accepted

    override def toString = "ClusterConfirmCoupling"


  final case class ClusterPassiveDown(activeId: NodeId, passiveId: NodeId)
  extends ClusterCommand:
    type Response = Response.Accepted
    assertThat(activeId != passiveId)
    override def toString = s"ClusterPassiveDown(activeId=$activeId passiveId=$passiveId)"

  final case class ClusterInhibitActivation(duration: FiniteDuration)
  extends ClusterCommand:
    type Response = ClusterInhibitActivation.Response
  object ClusterInhibitActivation:
    final case class Response(failedOver: Option[ClusterState.FailedOver])
    extends ClusterCommand.Response

    private implicit val x: Codec.AsObject[ClusterState.FailedOver] =
      deriveCodec[ClusterState.FailedOver]
    implicit val jsonCodec: Codec.AsObject[Response] = deriveCodec

  sealed trait Response
  object Response:
    sealed trait Accepted extends Response
    case object Accepted extends Accepted

    implicit val ResponseJsonCodec: TypedJsonCodec[Response] = TypedJsonCodec[Response](
      Subtype(Accepted),
      Subtype.named1[ClusterInhibitActivation.Response]("ClusterInhibitActivation.Response"))

  implicit val jsonCodec: TypedJsonCodec[ClusterCommand] = TypedJsonCodec[ClusterCommand](
    Subtype(deriveCodec[ClusterStartBackupNode]),
    Subtype(deriveCodec[ClusterPrepareCoupling]),
    Subtype(deriveCodec[ClusterCouple]),
    Subtype(deriveCodec[ClusterConfirmCoupling]),
    Subtype(deriveCodec[ClusterRecouple]),
    Subtype(deriveCodec[ClusterPassiveDown]),
    Subtype(deriveCodec[ClusterInhibitActivation]))

  intelliJuseImport(FiniteDurationJsonEncoder)
