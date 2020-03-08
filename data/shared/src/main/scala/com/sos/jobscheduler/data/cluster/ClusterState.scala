package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, ClusterCoupled, FailedOver, FollowerLost, FollowingStarted, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterState._
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, JournalPosition, JournaledState, KeyedEvent}
import monix.reactive.Observable
import scala.collection.immutable.Seq

sealed trait ClusterState
extends JournaledState[ClusterState, ClusterEvent]
{
  def maybeActiveUri: Option[Uri]

  def applyEvent(keyedEvent: KeyedEvent[ClusterEvent]): Checked[ClusterState] = {
    val event = keyedEvent.event
    if (!keyedEvent.key.isInstanceOf[NoKey])
      eventNotApplicable(keyedEvent)
    else
      (this, event) match {
        case (Empty, BecameSole(activeUri)) =>
          Right(Sole(activeUri))

        case (Sole(primaryUri), BackupNodeAppointed(backupUri)) if primaryUri != backupUri =>
          Right(AwaitingFollower(primaryUri :: backupUri :: Nil))

        case (Sole(primaryUri), FollowingStarted(followingUri)) if primaryUri != followingUri =>
          Right(AwaitingAppointment(primaryUri :: followingUri :: Nil))

        case (state: AwaitingFollower, FollowingStarted(followingUri)) if state.backupUri == followingUri =>
          Right(PreparedToBeCoupled(state.uris))

        case (state: AwaitingAppointment, BackupNodeAppointed(backupUri)) if state.backupUri == backupUri =>
          Right(PreparedToBeCoupled(state.uris))

        case (state: PreparedToBeCoupled, ClusterCoupled) =>
          Right(Coupled(state.uris, 0))

        case (state: Decoupled, ClusterCoupled) =>
          Right(Coupled(state.uris, state.active))

        case (state: Coupled, SwitchedOver(uri)) if uri == state.passiveUri =>
          Right(ProperlyDecoupled(state.uris, state.passive))

        case (state: Coupled, FollowerLost(uri)) if state.passiveUri == uri =>
          Right(ProperlyDecoupled(state.uris, state.active))

        case (state: Coupled, event: FailedOver)
          if state.activeUri == event.failedActiveUri && state.passiveUri == event.activatedUri =>
          Right(FailedOverDecoupled(state.uris, state.passive, event.failedAt))

        case (state: OtherFailedOver, event: FailedOver)
          if state.activeUri == event.activatedUri && state.passiveUri == event.failedActiveUri =>
          Right(FailedOverDecoupled(state.uris, state.active, event.failedAt))

        case (_, keyedEvent) => eventNotApplicable(keyedEvent)
      }
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.fromIterable((this != Empty) ? ClusterStateSnapshot(this))

  final def isActive(uri: Uri) = maybeActiveUri contains uri

  def isTheFollowingNode(uri: Uri) = false
}

object ClusterState
{
  final case class ClusterStateSnapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like Sole but own URI is unknown. Non-permanent state, not stored. */
  case object Empty extends ClusterState {
    def maybeActiveUri = None
  }

  sealed trait HasPrimaryNode extends ClusterState {
    def uris: Seq[Uri]
    def primaryUri = uris.head
    def active: Int

    final def activeUri = uris(active)
    final def maybeActiveUri = Some(activeUri)

    protected final def assertIsValid(): Unit = {
      val n = uris.length
      assertThat((n == 1 || n == 2) && uris.toSet.size == n && active >= 0 && active < n)
    }
  }

  sealed trait HasBackupNode extends HasPrimaryNode
  {
    final def backupUri = uris(1)
    final def passiveUri = uris(passive)
    final def passive = 1 - active
  }

  /** Sole node of the cluster.
    * Node is active without standby node. */
  final case class Sole(override val primaryUri: Uri)
  extends HasPrimaryNode {
    val uris = primaryUri :: Nil
    def active = 0
  }

  /** A passive node follows the active node, but it is not yet appointment.
    * The URI of the following (passive) node is still unknown. */
  final case class AwaitingAppointment(uris: Seq[Uri])
  extends HasBackupNode
  {
    assertIsValid()

    def active = 0

    override def toString = s"AwaitingAppointment(primary=$primaryUri, backup=$backupUri)"
  }

  final case class AwaitingFollower(uris: Seq[Uri])
  extends HasBackupNode
  {
    assertIsValid()

    def active = 0
  }

  /** Intermediate state only which is immediately followed by transition ClusterEvent.ClusterCoupled -> Coupled. */
  final case class PreparedToBeCoupled(uris: Seq[Uri])
  extends HasBackupNode
  {
    assertIsValid()

    def active = 0

    override def toString = s"PreparedToBeCoupled(primary=$primaryUri, backup=$backupUri)"
  }

  sealed trait CoupledOrDecoupled extends HasBackupNode {
    protected final def primaryBackupString = {
      val sb = new StringBuilder
      sb.append("primary=")
      sb.append(primaryUri.string)
      if (active == 0) sb.append(" active")
      sb.append(", backup=")
      sb.append(backupUri.string)
      if (active == 1) sb.append(" active")
      sb.toString
    }
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(uris: Seq[Uri], active: Int)
  extends CoupledOrDecoupled
  {
    assertIsValid()

    override def isTheFollowingNode(uri: Uri) = uri == passiveUri

    override def toString = s"Coupled($primaryBackupString)"
  }

  sealed trait Decoupled extends CoupledOrDecoupled

  final case class ProperlyDecoupled(uris: Seq[Uri], active: Int)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"ProperlyDecoupled($primaryBackupString)"
  }

  /** Decoupled after failover.
    * @param failedAt the failing nodes journal must be truncated at this point. */
  final case class FailedOverDecoupled(uris: Seq[Uri], active: Int, failedAt: JournalPosition)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"FailedOverDecoupled($primaryBackupString, $failedAt)"
  }

  /** Non-permanent state, not stored, used while retrieving the journal from the failed-over node. */
  final case class OtherFailedOver(uris: Seq[Uri], active: Int)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"FailedOverDecoupled($primaryBackupString)"
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
}
