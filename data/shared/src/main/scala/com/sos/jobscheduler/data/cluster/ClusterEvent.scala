package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.{JournalPosition, NoKeyEvent}

sealed trait ClusterEvent extends NoKeyEvent

object ClusterEvent
{
  final case class BecameSole(activeUri: Uri)
  extends ClusterEvent

  final case class BackupNodeAppointed(uri: Uri)
  extends ClusterEvent

  final case class FollowingStarted(followingUri: Uri)
  extends ClusterEvent

  sealed trait ClusterCoupled
  extends ClusterEvent
  case object ClusterCoupled
  extends ClusterCoupled

  final case class SwitchedOver(uri: Uri)
  extends ClusterEvent

  final case class FailedOver(failedActiveUri: Uri, activatedUri: Uri, failedAt: JournalPosition)
  extends ClusterEvent
  {
    override def toString = s"$FailedOver($activatedUri <- $failedActiveUri, $failedAt)"
  }

  final case class FollowerLost(uri: Uri)
  extends ClusterEvent

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype(deriveCodec[BecameSole]),
    Subtype(deriveCodec[BackupNodeAppointed]),
    Subtype(deriveCodec[FollowingStarted]),
    Subtype(ClusterCoupled),
    Subtype(deriveCodec[SwitchedOver]),
    Subtype(deriveCodec[FailedOver]),
    Subtype(deriveCodec[FollowerLost]))
}
