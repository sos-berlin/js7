package js7.data.cluster

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.base.utils.Assertions.assertThat
import js7.base.web.Uri
import js7.data.event.{JournalPosition, NoKeyEvent}
import js7.data.node.NodeId

sealed trait ClusterEvent extends NoKeyEvent

object ClusterEvent
{
  private type Id = NodeId

  final case class ClusterNodesAppointed(setting: ClusterSetting)
  extends ClusterEvent
  object ClusterNodesAppointed {
    def checked(setting: ClusterSetting): Checked[ClusterNodesAppointed] =
      Right(new ClusterNodesAppointed(setting.normalized))
  }

  final case class ClusterCouplingPrepared(activeId: Id)
  extends ClusterEvent
  {
    override def toString = s"ClusterCouplingPrepared(activeId=$activeId)"
  }

  final case class ClusterCoupled(activeId: Id)
  extends ClusterEvent
  {
    override def toString = s"ClusterCoupled(activeId=$activeId)"
  }

  final case class ClusterSwitchedOver(activatedId: Id)
  extends ClusterEvent

  sealed trait ClusterNodeLostEvent extends ClusterEvent {
    def lostNodeId: Id
  }

  final case class ClusterFailedOver(failedActiveId: Id, activatedId: Id, failedAt: JournalPosition)
  extends ClusterNodeLostEvent
  {
    def lostNodeId = failedActiveId

    override def toString = s"ClusterFailedOver($failedActiveId --> $activatedId, $failedAt)"
  }

  final case class ClusterPassiveLost(id: Id)
  extends ClusterNodeLostEvent {
    def lostNodeId = id
  }

  type ClusterActiveNodeShutDown = ClusterActiveNodeShutDown.type
  case object ClusterActiveNodeShutDown
  extends ClusterEvent

  type ClusterActiveNodeRestarted = ClusterActiveNodeRestarted.type
  case object ClusterActiveNodeRestarted
  extends ClusterEvent

  //case object ClusterAllNodesShutDown
  //extends ClusterEvent

  final case class ClusterSettingUpdated(
    passiveUri: Option[Uri] = None,
    clusterWatches: Option[Seq[ClusterSetting.Watch]] = None)
  extends ClusterEvent {
    assertThat(passiveUri.nonEmpty || clusterWatches.nonEmpty)
  }

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype(deriveCodec[ClusterNodesAppointed]),
    Subtype(deriveCodec[ClusterCouplingPrepared]),
    Subtype(deriveCodec[ClusterCoupled]),
    Subtype(deriveCodec[ClusterSwitchedOver]),
    Subtype(deriveCodec[ClusterFailedOver]),
    Subtype(deriveCodec[ClusterPassiveLost]),
    Subtype(ClusterActiveNodeShutDown),
    Subtype(ClusterActiveNodeRestarted),
    Subtype(deriveCodec[ClusterSettingUpdated]))
}
