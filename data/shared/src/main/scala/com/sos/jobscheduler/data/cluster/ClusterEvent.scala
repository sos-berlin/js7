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

  final case class FollowingStarted(uri: Uri)
  extends ClusterEvent

  //type PreparedToBeCoupled = PreparedToBeCoupled.type
  //case object PreparedToBeCoupled extends ClusterEvent

  type Coupled = Coupled.type
  case object Coupled extends ClusterEvent

  final case class SwitchedOver(uri: Uri)
  extends ClusterEvent

  final case class FailedOver(failedActiveUri: Uri, activatedUri: Uri, failedAt: JournalPosition)
  extends ClusterEvent
  {
    override def toString = s"$FailedOver($failedActiveUri --> $activatedUri, $failedAt)"
  }

  final case class FollowerLost(uri: Uri)
  extends ClusterEvent

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype.named(deriveCodec[BecameSole]         , "Cluster.BecameSole"),
    Subtype.named(deriveCodec[BackupNodeAppointed], "Cluster.BackupNodeAppointed"),
    Subtype.named(deriveCodec[FollowingStarted]   , "Cluster.FollowingStarted"),
    Subtype.named(Coupled                         , "Cluster.Coupled"),
    Subtype.named(deriveCodec[SwitchedOver]       , "Cluster.SwitchedOver"),
    Subtype.named(deriveCodec[FailedOver]         , "Cluster.FailedOver"),
    Subtype.named(deriveCodec[FollowerLost]       , "Cluster.FollowerLost"))
}
