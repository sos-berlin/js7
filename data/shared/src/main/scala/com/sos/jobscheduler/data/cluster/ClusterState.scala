package com.sos.jobscheduler.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, FollowingStarted}
import com.sos.jobscheduler.data.cluster.ClusterState._
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, JournaledState, KeyedEvent}
import monix.reactive.Observable

sealed trait ClusterState
extends JournaledState[ClusterState, ClusterEvent]
{
  def applyEvent = new PartialFunction[KeyedEvent[ClusterEvent], Checked[ClusterState]] {
    def isDefinedAt(keyedEvent: KeyedEvent[ClusterEvent]) =
      keyedEvent match {
        case KeyedEvent(NoKey, event) => transition.isDefinedAt((ClusterState.this, event))
        case _ => false
      }

    def apply(keyedEvent: KeyedEvent[ClusterEvent]) =
      transition((ClusterState.this, keyedEvent.event))
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.pure(Snapshot(this))
}

object ClusterState
{
  final case class Snapshot(clusterState: ClusterState)

  sealed trait HasActiveNode extends ClusterState {
    def activeNodeId: ClusterNodeId
  }

  /** Sole node of the cluster.
    * Node is active without standby node. */
  final case class Sole(activeNodeId: ClusterNodeId)
  extends HasActiveNode

  /** State only temporarily known to an initial backup node.
    * State is never stored as a snapshot. */
  final case class InitialBackupNode(activeUri: Uri)
  extends ClusterState

  /** A passive node follows the active node, but it is not yet appointment.
    * The URI of the following (passive) node is still unknown. */
  final case class AwaitingAppointment(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId)
  extends HasActiveNode
  {
    assertThat(activeNodeId != passiveNodeId)
  }

  final case class AwaitingFollower(activeNodeId: ClusterNodeId, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends HasActiveNode
  {
    assertThat(activeNodeId != passiveNodeId)
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(activeNodeId: ClusterNodeId, activeUri: Uri, passiveNodeId: ClusterNodeId, passiveUri: Uri)
  extends HasActiveNode
  {
    assertThat(activeNodeId != passiveNodeId)
    assertThat(activeUri != passiveUri)
  }

  implicit val jsonCodec = TypedJsonCodec[ClusterState](
    Subtype(deriveCodec[InitialBackupNode]),
    Subtype(deriveCodec[Sole]),
    Subtype(deriveCodec[AwaitingAppointment]),
    Subtype(deriveCodec[AwaitingFollower]),
    Subtype(deriveCodec[Coupled]))

  private val transition: PartialFunction[(ClusterState, ClusterEvent), Checked[ClusterState]] = {
    case (Sole(nodeId), BackupNodeAppointed(passiveNodeId, passiveUri)) =>
      ensureDifferentNodeIds(nodeId, passiveNodeId) >>
        Right(AwaitingFollower(nodeId, passiveNodeId, passiveUri))

    case (Sole(nodeId), FollowingStarted(followingNodeId, activeUri)) =>
      Right(AwaitingAppointment(nodeId, activeUri, followingNodeId))

    case (AwaitingFollower(activeNodeId, appointedNodeId, appointedUri), FollowingStarted(followingNodeId, activeUri))
      if appointedNodeId == followingNodeId =>
        ensureDifferentUris(activeUri, appointedUri) >>
        ensureDifferentNodeIds(activeNodeId, followingNodeId) >>
          Right(Coupled(activeNodeId, activeUri, appointedNodeId, appointedUri))

    case (AwaitingAppointment(activeNodeId, activeUri, followingNodeId), BackupNodeAppointed(appointedNodeId, appointedUri))
      if followingNodeId == appointedNodeId =>
        ensureDifferentUris(activeUri, appointedUri) >>
        ensureDifferentNodeIds(activeNodeId, appointedNodeId) >>
          Right(Coupled(activeNodeId, activeUri, appointedNodeId, appointedUri))

    //case (AwaitCoupling(_, Right((passiveNodeId, passiveUri))), BackupNodeAppointed(nodeId, uri))
    //  if passiveNodeId == nodeId || passiveUri == uri =>
    //    Left(Problem(s"Backup node '$uri' nodeId=$nodeId has already been added"))
  }

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
