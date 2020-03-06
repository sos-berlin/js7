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
  def maybeActiveNode: Option[Uri]

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
          Right(Coupled(activeUri, passiveUri))

        case (state: Decoupled, FollowingStarted(followingUri)) if followingUri == state.passiveUri =>
          Right(PreparedToBeCoupled(
            activeUri = state.activeUri,
            passiveUri = state.passiveUri))

        case (state: Coupled, SwitchedOver(uri)) if state.passiveUri == uri =>
          Right(ProperlyDecoupled(
            activeUri = uri,
            passiveUri = state.activeUri))

        case (state: Coupled, FollowerLost(uri)) if state.passiveUri == uri =>
          Right(ProperlyDecoupled(
            activeUri = state.activeUri,
            passiveUri = uri))

        case (state: Coupled, event: FailedOver)
          if state.activeUri == event.failedActiveUri && state.passiveUri == event.activatedUri =>
          Right(FailedOverDecoupled(
            activeUri = event.activatedUri,
            passiveUri = event.failedActiveUri,
            event.failedAt))

        case (state: OtherFailedOver, event: FailedOver)
          if state.activeUri == event.activatedUri && state.passiveUri == event.failedActiveUri =>
          Right(FailedOverDecoupled(
            activeUri = event.activatedUri,
            passiveUri = event.failedActiveUri,
            event.failedAt))

        case (_, keyedEvent) => eventNotApplicable(keyedEvent)
      }
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.fromIterable((this != Empty) ? ClusterStateSnapshot(this))

  final def isActive(uri: Uri) = maybeActiveNode contains uri

  def isTheFollowingNode(uri: Uri) = false
}

object ClusterState
{
  final case class ClusterStateSnapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like Sole but own URI is unknown. Non-permanent state, not stored. */
  case object Empty extends ClusterState {
    def maybeActiveNode = None
    def maybePassiveNode = None
  }

  sealed trait HasActiveNode extends ClusterState {
    def activeUri: Uri
    final def maybeActiveNode = Some(activeUri)
  }

  sealed trait HasPassiveNode extends HasActiveNode
  {
    def passiveUri: Uri

    final def maybePassiveNode = Some(passiveUri)
  }

  /** Sole node of the cluster.
    * Node is active without standby node. */
  final case class Sole(activeUri: Uri)
  extends HasActiveNode {
    def maybePassiveNode = None
  }

  /** A passive node follows the active node, but it is not yet appointment.
    * The URI of the following (passive) node is still unknown. */
  final case class AwaitingAppointment(activeUri: Uri, passiveUri: Uri)
  extends HasPassiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  final case class AwaitingFollower(activeUri: Uri, passiveUri: Uri)
  extends HasPassiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  /** Intermediate state only which is immediately followed by transition ClusterEvent.ClusterCoupled -> Coupled. */
  final case class PreparedToBeCoupled(activeUri: Uri, passiveUri: Uri)
  extends HasPassiveNode
  {
    assertThat(activeUri != passiveUri)
  }

  sealed trait CoupledOrDecoupled extends HasPassiveNode

  /** An active node is coupled with a passive node. */
  final case class Coupled(activeUri: Uri, passiveUri: Uri)
  extends CoupledOrDecoupled
  {
    assertThat(activeUri != passiveUri)

    override def isTheFollowingNode(uri: Uri) = uri == passiveUri
  }

  sealed trait Decoupled extends CoupledOrDecoupled

  final case class ProperlyDecoupled(activeUri: Uri, passiveUri: Uri)
  extends Decoupled
  {
    assertThat(activeUri != passiveUri)
  }

  /** Decoupled after failover.
    * @param failedAt the failing nodes journal must be truncated at this point. */
  final case class FailedOverDecoupled(activeUri: Uri, passiveUri: Uri, failedAt: JournalPosition)
  extends Decoupled
  {
    assertThat(activeUri != passiveUri)
  }

  /** Non-permanent state, not stored, used while retrieving the journal from the failed-over node. */
  final case class OtherFailedOver(activeUri: Uri, passiveUri: Uri)
  extends HasPassiveNode
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
    Subtype(deriveCodec[ProperlyDecoupled]),
    Subtype(deriveCodec[FailedOverDecoupled]),
    Subtype(deriveCodec[OtherFailedOver]))

  private def ensureDifferentUris(a: Uri, b: Uri, event: ClusterEvent): Checked[Unit] =
    if (a != b) Checked.unit
    else Left(Problem(s"Two cluster nodes must not have the same URI: $event"))
}
