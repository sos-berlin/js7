package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.NoKeyEvent

sealed trait ClusterEvent extends NoKeyEvent

object ClusterEvent
{
  final case class BecameSole(activeNodeId: ClusterNodeId)
  extends ClusterEvent

  final case class BackupNodeAppointed(nodeId: ClusterNodeId, uri: Uri)
  extends ClusterEvent

  final case class FollowingStarted(passiveNodeId: ClusterNodeId, activeUri: Uri)
  extends ClusterEvent

  sealed trait ClusterCoupled
  extends ClusterEvent
  case object ClusterCoupled
  extends ClusterCoupled

  final case class SwitchedOver(nodeId: ClusterNodeId)
  extends ClusterEvent

  //final case class Decoupled(passiveNodeId: ClusterNodeId) extends ClusterEvent
  //
  ///** Passive node has acknowledged last written event.
  //  * Events are valid now.
  //  * JobScheduler should process these events now.
  //  */
  //case object ClusterCommitted extends ClusterEvent
  //
  ///** Passive node has not acknowledged in time. */
  //final case class PassiveCommitTimedOut(nodeId: ClusterNodeId, elapsed: FiniteDuration) extends ClusterEvent
  //
  ///** The absolute majority of Agents says, this node is reachable.
  //  * Therefore this node shall stay or become active. */
  //case object MajorityForMe extends ClusterEvent
  //
  ///** The absolute majority of Agents do not say, this node is reachable, but another node is reachable.
  //  * After a determined duration, the other node shall stay or become active.
  //  */
  //final case class MajorityFor(active: ClusterNodeId) extends ClusterEvent

  implicit val jsonCodec = TypedJsonCodec[ClusterEvent](
    Subtype(deriveCodec[BecameSole]),
    Subtype(deriveCodec[BackupNodeAppointed]),
    Subtype(deriveCodec[FollowingStarted]),
    Subtype(ClusterCoupled),
    Subtype(deriveCodec[SwitchedOver]))
}
