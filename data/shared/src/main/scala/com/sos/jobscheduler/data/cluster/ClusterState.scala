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
import com.sos.jobscheduler.data.event.{EventId, JournalPosition, JournaledState, KeyedEvent}
import monix.reactive.Observable

sealed trait ClusterState
extends JournaledState[ClusterState, ClusterEvent]
{
  def applyEvent(keyedEvent: KeyedEvent[ClusterEvent]): Checked[ClusterState] = {
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
          Right(Coupled(activeUri, passiveUri, failedAt = None))

        case (state: Decoupled, FollowingStarted(followingUri)) if followingUri == state.passiveUri =>
          Right(PreparedToBeCoupled(
            activeUri = state.activeUri,
            passiveUri = state.passiveUri))

        case (state: Coupled, SwitchedOver(uri)) if state.passiveUri == uri =>
          Right(Decoupled(
            activeUri = uri,
            passiveUri = state.activeUri,
            failedAt = None))

        case (state: Coupled, event: FailedOver)
          if state.activeUri == event.failedActiveUri && state.passiveUri == event.activatedUri =>
          Right(Decoupled(
            activeUri = event.activatedUri,
            passiveUri = event.failedActiveUri,
            Some(event.failedAt)))

        case (state: OtherFailedOver, event: FailedOver)
          if state.activeUri == event.activatedUri && state.passiveUri == event.failedActiveUri =>
          Right(Decoupled(
            activeUri = event.activatedUri,
            passiveUri = event.failedActiveUri,
            Some(event.failedAt)))

        case (state: Coupled, FollowerLost(uri)) if state.passiveUri == uri =>
          Right(Decoupled(
            activeUri = state.activeUri,
            passiveUri = uri,
            failedAt = None))

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
    * Like Sole but own URI is unknown. Non-permanent state, not stored. */
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

  /** Sole node of the cluster.
    * Node is active without standby node. */
  final case class Sole(activeUri: Uri)
  extends HasActiveNode

  /** A passive node follows the active node, but it is not yet appointment.
    * The URI of the following (passive) node is still unknown. */
  final case class AwaitingAppointment(activeUri: Uri, passiveUri: Uri)
  extends HasActiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  final case class AwaitingFollower(activeUri: Uri, passiveUri: Uri)
  extends HasActiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  /** Intermediate state only which is immediately followed by transition ClusterEvent.ClusterCoupled -> Coupled. */
  final case class PreparedToBeCoupled(activeUri: Uri, passiveUri: Uri)
  extends HasActiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  sealed trait CoupledOrDecoupled extends HasActiveNode
  {
    def passiveUri: Uri
    def failedAt: Option[JournalPosition]
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(activeUri: Uri, passiveUri: Uri, failedAt: Option[JournalPosition])
  extends CoupledOrDecoupled
  {
    assertThat(activeUri != passiveUri)

    override def isTheFollowingNode(uri: Uri) = uri == passiveUri
  }

  final case class Decoupled(activeUri: Uri, passiveUri: Uri, failedAt: Option[JournalPosition])
  extends CoupledOrDecoupled
  {
    assertThat(activeUri != passiveUri)
  }

  /** Non-permanent state, not stored, used while retrieving the journal from the failed-over node. */
  final case class OtherFailedOver(activeUri: Uri, passiveUri: Uri)
  extends HasActiveNode
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
  //Subtype(deriveCodec[OtherFaileOver]))

  private def ensureDifferentUris(a: Uri, b: Uri, event: ClusterEvent): Checked[Unit] =
    if (a != b) Checked.unit
    else Left(Problem(s"Two cluster nodes must not have the same URI: $event"))
}
