package com.sos.jobscheduler.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.data.cluster.ClusterSetting.checkUris
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.{JournalPosition, NoKeyEvent}

sealed trait ClusterEvent extends NoKeyEvent

object ClusterEvent
{
  private type Id = ClusterNodeId

  final case class NodesAppointed(idToUri: Map[Id, Uri], activeId: Id)
  extends ClusterEvent
  {
    checkUris(idToUri, activeId).orThrow
  }
  object NodesAppointed {
    def checked(idToUri: Map[Id, Uri], activeId: Id): Checked[NodesAppointed] =
      checkUris(idToUri, activeId) >>
        Checked(new NodesAppointed(idToUri, activeId))
  }

  final case class CouplingPrepared(activeId: Id)
  extends ClusterEvent

  final case class Coupled(activeId: Id)
  extends ClusterEvent

  final case class SwitchedOver(toId: Id)
  extends ClusterEvent

  final case class FailedOver(failedActiveId: Id, activatedId: Id, failedAt: JournalPosition)
  extends ClusterEvent
  {
    override def toString = s"$FailedOver($failedActiveId --> $activatedId, $failedAt)"
  }

  final case class PassiveLost(id: Id)
  extends ClusterEvent

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype.named(deriveCodec[NodesAppointed]  , "Cluster.NodesAppointed"),
    Subtype.named(deriveCodec[CouplingPrepared], "Cluster.CouplingPrepared"),
    Subtype.named(deriveCodec[Coupled]         , "Cluster.Coupled"),
    Subtype.named(deriveCodec[SwitchedOver]    , "Cluster.SwitchedOver"),
    Subtype.named(deriveCodec[FailedOver]      , "Cluster.FailedOver"),
    Subtype.named(deriveCodec[PassiveLost]     , "Cluster.PassiveLost"))
}
