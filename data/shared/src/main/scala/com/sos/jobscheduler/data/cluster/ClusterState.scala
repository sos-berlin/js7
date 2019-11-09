package com.sos.jobscheduler.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, ClusterCoupled, FollowingStarted, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState._
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, JournaledState, KeyedEvent}
import monix.reactive.Observable

sealed trait ClusterState
extends JournaledState[ClusterState, ClusterEvent]
{
  def applyEvent(keyedEvent: KeyedEvent[ClusterEvent]) = {
    val event = keyedEvent.event
    if (!keyedEvent.key.isInstanceOf[NoKey])
      eventNotApplicable(keyedEvent)
    else
      (this, event) match {
        case (Empty, BecameSole(activeNodeId)) =>
          Right(Sole(activeNodeId))

        case (Sole(nodeId), BackupNodeAppointed(passiveNodeId, passiveUri)) if nodeId != passiveNodeId =>
          Right(AwaitingFollower(nodeId, passiveNodeId, passiveUri))

        case (Sole(nodeId), FollowingStarted(followingNodeId, activeUri)) if nodeId != followingNodeId =>
          Right(AwaitingAppointment(nodeId, activeUri, followingNodeId))

        case (AwaitingFollower(activeNodeId, appointedNodeId, appointedUri), FollowingStarted(followingNodeId, activeUri))
          if appointedNodeId == followingNodeId =>
            ensureDifferentUris(activeUri, appointedUri, event) >>
              Right(PreparedToBeCoupled(activeNodeId, activeUri, appointedNodeId, appointedUri))

        case (AwaitingAppointment(activeNodeId, activeUri, followingNodeId), BackupNodeAppointed(appointedNodeId, appointedUri))
          if followingNodeId == appointedNodeId =>
            ensureDifferentUris(activeUri, appointedUri, event) >>
              Right(PreparedToBeCoupled(activeNodeId, activeUri, appointedNodeId, appointedUri))

        case (PreparedToBeCoupled(activeNodeId, activeUri, passiveNodeId, passiveUri), ClusterCoupled) =>
          Right(Coupled(activeNodeId, activeUri, passiveNodeId, passiveUri))

        case (state: Coupled, SwitchedOver(nodeId)) if state.passiveNodeId == nodeId =>
          Right(Decoupled(
            activeNodeId = nodeId, activeUri = state.passiveUri,
            passiveNodeId = state.activeNodeId, passiveUri = state.activeUri))

        case (state: Decoupled, FollowingStarted(followingNodeId, activeUri))
          if followingNodeId == state.passiveNodeId && activeUri == state.activeUri =>
            Right(PreparedToBeCoupled(
              activeNodeId = state.activeNodeId, activeUri = state.activeUri,
              passiveNodeId = state.passiveNodeId, passiveUri = state.passiveUri))

        case (state: Decoupled, FollowingStarted(followingNodeId, activeUri))
          if followingNodeId == state.activeNodeId && activeUri == state.activeUri =>
            Right(PreparedToBeCoupled(
              activeNodeId = state.passiveNodeId, activeUri = state.passiveUri,
              passiveNodeId = state.activeNodeId, passiveUri = state.activeUri))

        case (_, keyedEvent) => eventNotApplicable(keyedEvent)
      }
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.fromIterable((this != Empty) ? ClusterStateSnapshot(this))

  def isActive(nodeId: ClusterNodeId): Boolean

  def isPassive(nodeId: ClusterNodeId): Boolean
}

object ClusterState
{
  final case class ClusterStateSnapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like Sole but the NodeId is unknown. */
  case object Empty extends ClusterState {
    def isActive(nodeId: ClusterNodeId) = false
    def isPassive(nodeId: ClusterNodeId) = false
  }

  sealed trait HasActiveNode extends ClusterState {
    def activeNodeId: ClusterNodeId
    def isActive(nodeId: ClusterNodeId) = nodeId == activeNodeId
  }
  //object HasActiveNode {
  //  def unapply(state: ClusterState): Option[ClusterNodeId] =
  //    state match {
  //      case state: HasActiveNode => Some(state.activeNodeId)
  //      case _ => None
  //    }
  //}

  sealed trait HasPassiveNode extends ClusterState {
    def passiveNodeId: ClusterNodeId
    def isPassive(nodeId: ClusterNodeId) = nodeId == passiveNodeId
  }

  /** Sole node of the cluster.
    * Node is active without standby node. */
  final case class Sole(activeNodeId: ClusterNodeId)
  extends HasActiveNode {
    def isPassive(nodeId: ClusterNodeId) = false
  }

  /** A passive node follows the active node, but it is not yet appointment.
    * The URI of the following (passive) node is still unknown. */
  final case class AwaitingAppointment(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId)
  extends HasActiveNode with HasPassiveNode
  {
    assertThat(activeNodeId != passiveNodeId)
  }

  final case class AwaitingFollower(activeNodeId: ClusterNodeId, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends HasActiveNode with HasPassiveNode
  {
    assertThat(activeNodeId != passiveNodeId)
  }

  /** Intermediate state only which is immediately followed by transition ClusterEvent.ClusterCoupled -> Coupled. */
  final case class PreparedToBeCoupled(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends HasActiveNode with HasPassiveNode
  {
    assertThat(activeNodeId != passiveNodeId)
    assertThat(activeUri != passiveUri)
  }

  sealed trait CoupledOrDecoupled extends HasActiveNode with HasPassiveNode
  {
    def activeUri: Uri
    def passiveUri: Uri
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends CoupledOrDecoupled
  {
    assertThat(activeNodeId != passiveNodeId)
    assertThat(activeUri != passiveUri)
  }

  final case class Decoupled(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends CoupledOrDecoupled
  {
    assertThat(activeNodeId != passiveNodeId)
    assertThat(activeUri != passiveUri)
  }

  implicit val jsonCodec = TypedJsonCodec[ClusterState](
    Subtype(Empty),
    Subtype(deriveCodec[Sole]),
    Subtype(deriveCodec[AwaitingAppointment]),
    Subtype(deriveCodec[AwaitingFollower]),
    Subtype(deriveCodec[PreparedToBeCoupled]),
    Subtype(deriveCodec[Coupled]),
    Subtype(deriveCodec[Decoupled]))

  private def ensureDifferentUris(a: Uri, b: Uri, event: ClusterEvent): Checked[Unit] =
    if (a != b) Checked.unit
    else Left(Problem(s"Two cluster nodes must not have the same URI: $event"))
}
