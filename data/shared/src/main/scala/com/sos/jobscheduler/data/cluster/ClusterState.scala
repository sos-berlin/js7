package com.sos.jobscheduler.data.cluster

import cats.instances.either._
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, ClusterCoupled, FailedOver, FollowerLost, FollowingStarted, SwitchedOver}
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
        case (Empty, BecameSole(activeUri)) =>
          Right(Sole(activeUri))

        case (Sole(activeUri), BackupNodeAppointed(passiveUri)) if activeUri != passiveUri =>
          Right(AwaitingFollower(activeUri, passiveUri))

        case (Sole(activeUri), FollowingStarted(followingUri)) if activeUri != followingUri =>
          Right(AwaitingAppointment(activeUri, followingUri))

        case (AwaitingFollower(activeUri, appointedUri), FollowingStarted(followingUri)) if appointedUri == followingUri =>
          ensureDifferentUris(activeUri, appointedUri, event) >>
            Right(PreparedToBeCoupled(activeUri, appointedUri))

        case (AwaitingAppointment(activeUri, followingUri), BackupNodeAppointed(appointedUri)) if followingUri == appointedUri =>
          ensureDifferentUris(activeUri, appointedUri, event) >>
            Right(PreparedToBeCoupled(activeUri, appointedUri))

        case (PreparedToBeCoupled(activeUri, passiveUri), ClusterCoupled) =>
          Right(Coupled(activeUri, passiveUri))

        case (state: Decoupled, FollowingStarted(followingUri)) if followingUri == state.passiveUri =>
          Right(PreparedToBeCoupled(
            activeUri = state.activeUri,
            passiveUri = state.passiveUri))

        case (state: Decoupled, FollowingStarted(followingUri)) if followingUri == state.activeUri =>
          Right(PreparedToBeCoupled(
            activeUri = state.passiveUri,
            passiveUri = state.activeUri))

        case (state: Coupled, SwitchedOver(uri)) if state.passiveUri == uri =>
          Right(Decoupled(
            activeUri = uri,
            passiveUri = state.activeUri))

        case (state: Coupled, event: FailedOver)
          if state.activeUri == event.failedActiveUri && state.passiveUri == event.activatedUri =>
          Right(Decoupled(
            activeUri = event.activatedUri,
            passiveUri = event.failedActiveUri))

        case (state: Coupled, FollowerLost(uri)) if state.passiveUri == uri =>
          Right(Decoupled(
            activeUri = state.activeUri,
            passiveUri = uri))

        case (_, keyedEvent) => eventNotApplicable(keyedEvent)
      }
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.fromIterable((this != Empty) ? ClusterStateSnapshot(this))

  def isActive(uri: Uri): Boolean

  def isTheFollowingNode(uri: Uri) = false
}

object ClusterState
{
  final case class ClusterStateSnapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like Sole but own URI is unknown. */
  case object Empty extends ClusterState {
    def isActive(uri: Uri) = false
  }

  sealed trait HasActiveNode extends ClusterState {
    def activeUri: Uri
    def isActive(uri: Uri) = uri == activeUri
  }
  //object HasActiveNode {
  //  def unapply(state: ClusterState): Option[Uri] =
  //    state match {
  //      case state: HasActiveNode => Some(state.activeUri)
  //      case _ => None
  //    }
  //}

  sealed trait HasPassiveNode extends ClusterState {
    def passiveUri: Uri
  }

  /** Sole node of the cluster.
    * Node is active without standby node. */
  final case class Sole(activeUri: Uri)
  extends HasActiveNode

  /** A passive node follows the active node, but it is not yet appointment.
    * The URI of the following (passive) node is still unknown. */
  final case class AwaitingAppointment(activeUri: Uri, passiveUri: Uri)
  extends HasActiveNode with HasPassiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  final case class AwaitingFollower(activeUri: Uri, passiveUri: Uri)
  extends HasActiveNode with HasPassiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  /** Intermediate state only which is immediately followed by transition ClusterEvent.ClusterCoupled -> Coupled. */
  final case class PreparedToBeCoupled(activeUri: Uri, passiveUri: Uri)
  extends HasActiveNode with HasPassiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  sealed trait CoupledOrDecoupled extends HasActiveNode with HasPassiveNode
  {
    def activeUri: Uri
    def passiveUri: Uri
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(activeUri: Uri, passiveUri: Uri)
  extends CoupledOrDecoupled
  {
    assertThat(activeUri != passiveUri)

    override def isTheFollowingNode(uri: Uri) = uri == passiveUri
  }

  final case class Decoupled(activeUri: Uri, passiveUri: Uri)
  extends CoupledOrDecoupled
  {
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
