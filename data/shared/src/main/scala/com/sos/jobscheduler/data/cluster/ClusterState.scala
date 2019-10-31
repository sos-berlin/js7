package com.sos.jobscheduler.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Assertions.assertThat
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
    if (!keyedEvent.key.isInstanceOf[NoKey])
      eventNotApplicable(keyedEvent)
    else
      (this, keyedEvent.event) match {
        case (Empty, BecameSole(activeNodeId)) =>
          Right(Sole(activeNodeId))

        case (Sole(nodeId), BackupNodeAppointed(passiveNodeId, passiveUri)) =>
          ensureDifferentNodeIds(nodeId, passiveNodeId) >>
            Right(AwaitingFollower(nodeId, passiveNodeId, passiveUri))

        case (Sole(nodeId), FollowingStarted(followingNodeId, activeUri)) =>
          Right(AwaitingAppointment(nodeId, activeUri, followingNodeId))

        case (AwaitingFollower(activeNodeId, appointedNodeId, appointedUri), FollowingStarted(followingNodeId, activeUri))
          if appointedNodeId == followingNodeId =>
            ensureDifferentUris(activeUri, appointedUri) >>
            ensureDifferentNodeIds(activeNodeId, followingNodeId) >>
              Right(PreparedToBeCoupled(activeNodeId, activeUri, appointedNodeId, appointedUri))

        case (AwaitingAppointment(activeNodeId, activeUri, followingNodeId), BackupNodeAppointed(appointedNodeId, appointedUri))
          if followingNodeId == appointedNodeId =>
            ensureDifferentUris(activeUri, appointedUri) >>
            ensureDifferentNodeIds(activeNodeId, appointedNodeId) >>
              Right(PreparedToBeCoupled(activeNodeId, activeUri, appointedNodeId, appointedUri))

        case (PreparedToBeCoupled(activeNodeId, activeUri, passiveNodeId, passiveUri), ClusterCoupled) =>
          Right(Coupled(activeNodeId, activeUri, passiveNodeId, passiveUri))

        case (state: Coupled, SwitchedOver(nodeId)) if state.passiveNodeId == nodeId =>
          Right(Sole(nodeId))

        case (_, keyedEvent) => eventNotApplicable(keyedEvent)
      }
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.pure(Snapshot(this))
}

object ClusterState
{
  final case class Snapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like Sole but the NodeId is unknown. */
  case object Empty extends ClusterState

  /** Sole node of the cluster.
    * Node is active without standby node. */
  final case class Sole(nodeId: ClusterNodeId) extends ClusterState

  /** A passive node follows the active node, but it is not yet appointment.
    * The URI of the following (passive) node is still unknown. */
  final case class AwaitingAppointment(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId)
  extends ClusterState
  {
    assertThat(activeNodeId != passiveNodeId)
  }

  final case class AwaitingFollower(activeNodeId: ClusterNodeId, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends ClusterState
  {
    assertThat(activeNodeId != passiveNodeId)
  }

  /** Intermediate state only which is immediately followed by transition ClusterEvent.ClusterCoupled -> Coupled. */
  final case class PreparedToBeCoupled(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends ClusterState
  {
    assertThat(activeNodeId != passiveNodeId)
    assertThat(activeUri != passiveUri)
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends ClusterState
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
    Subtype(deriveCodec[Coupled]))

  private def ensureDifferentUris(a: Uri, b: Uri): Checked[Unit] =
    if (a == b)
      Left(Problem(s"Two cluster nodes must not have the same URI: $a"))
    else
      Checked.unit

  private def ensureDifferentNodeIds(a: ClusterNodeId, b: ClusterNodeId): Checked[Unit] =
    if (a == b)
      Left(Problem(s"Two cluster nodes must not have the same ClusterNodeId: $a"))
    else
      Checked.unit
}
