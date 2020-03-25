package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.{JournalPosition, NoKeyEvent}

sealed trait ClusterEvent extends NoKeyEvent

object ClusterEvent
{
  final case class BecameSole(primaryUri: Uri)
  extends ClusterEvent

  final case class BackupNodeAppointed(uri: Uri)
  extends ClusterEvent

  final case class CouplingPrepared(passiveUri: Uri)
  extends ClusterEvent

  type Coupled = Coupled.type
  case object Coupled extends ClusterEvent

  final case class SwitchedOver(uri: Uri)
  extends ClusterEvent

  final case class FailedOver(failedActiveUri: Uri, activatedUri: Uri, failedAt: JournalPosition)
  extends ClusterEvent
  {
    override def toString = s"$FailedOver($failedActiveUri --> $activatedUri, $failedAt)"
  }

  final case class PassiveLost(uri: Uri)
  extends ClusterEvent

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype.named(deriveCodec[BecameSole]         , "Cluster.BecameSole"),
    Subtype.named(deriveCodec[BackupNodeAppointed], "Cluster.BackupNodeAppointed"),
    Subtype.named(deriveCodec[CouplingPrepared]   , "Cluster.CouplingPrepared"),
    Subtype.named(Coupled                         , "Cluster.Coupled"),
    Subtype.named(deriveCodec[SwitchedOver]       , "Cluster.SwitchedOver"),
    Subtype.named(deriveCodec[FailedOver]         , "Cluster.FailedOver"),
    Subtype.named(deriveCodec[PassiveLost]        , "Cluster.PassiveLost"))
}
