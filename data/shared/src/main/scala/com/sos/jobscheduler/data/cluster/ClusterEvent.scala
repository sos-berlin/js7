package com.sos.jobscheduler.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.cluster.ClusterSetting.checkUris
import com.sos.jobscheduler.data.event.{JournalPosition, NoKeyEvent}

sealed trait ClusterEvent extends NoKeyEvent

object ClusterEvent
{
  private type Id = ClusterNodeId

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

  final case class ClusterSwitchedOver(toId: Id)
  extends ClusterEvent

  final case class ClusterFailedOver(failedActiveId: Id, activatedId: Id, failedAt: JournalPosition)
  extends ClusterEvent
  {
    override def toString = s"ClusterFailedOver($failedActiveId --> $activatedId, $failedAt)"
  }

  final case class ClusterPassiveLost(id: Id)
  extends ClusterEvent

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype(deriveCodec[ClusterNodesAppointed]),
    Subtype(deriveCodec[ClusterCouplingPrepared]),
    Subtype(deriveCodec[ClusterCoupled]),
    Subtype(deriveCodec[ClusterSwitchedOver]),
    Subtype(deriveCodec[ClusterFailedOver]),
    Subtype(deriveCodec[ClusterPassiveLost]))
}
