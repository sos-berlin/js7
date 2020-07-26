package js7.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.web.Uri
import js7.data.cluster.ClusterSetting.checkUris
import js7.data.event.{JournalPosition, NoKeyEvent}
import js7.data.node.NodeId

sealed trait ClusterEvent extends NoKeyEvent

object ClusterEvent
{
  private type Id = NodeId

  final case class ClusterNodesAppointed(idToUri: Map[Id, Uri], activeId: Id)
  extends ClusterEvent
  {
    checkUris(idToUri, activeId).orThrow
  }
  object ClusterNodesAppointed {
    def checked(idToUri: Map[Id, Uri], activeId: Id): Checked[ClusterNodesAppointed] =
      checkUris(idToUri, activeId) >>
        Checked(
          new ClusterNodesAppointed(
            idToUri
              // Primary node should be first (possible for optimized short Scala Map)
              .toVector.sortBy(o => if (o._1 == activeId) 0 else 1).toMap,
            activeId))
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

  final case class ClusterFailedOver(failedActiveId: Id, activatedId: Id, failedAt: JournalPosition)
  extends ClusterEvent
  {
    override def toString = s"ClusterFailedOver($failedActiveId --> $activatedId, $failedAt)"
  }

  final case class ClusterPassiveLost(id: Id)
  extends ClusterEvent

  type ClusterActiveNodeShutDown = ClusterActiveNodeShutDown.type
  case object ClusterActiveNodeShutDown
  extends ClusterEvent

  type ClusterActiveNodeRestarted = ClusterActiveNodeRestarted.type
  case object ClusterActiveNodeRestarted
  extends ClusterEvent

  //case object ClusterAllNodesShutDown
  //extends ClusterEvent

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype(deriveCodec[ClusterNodesAppointed]),
    Subtype(deriveCodec[ClusterCouplingPrepared]),
    Subtype(deriveCodec[ClusterCoupled]),
    Subtype(deriveCodec[ClusterSwitchedOver]),
    Subtype(deriveCodec[ClusterFailedOver]),
    Subtype(deriveCodec[ClusterPassiveLost]),
    Subtype(ClusterActiveNodeShutDown),
    Subtype(ClusterActiveNodeRestarted))
}
