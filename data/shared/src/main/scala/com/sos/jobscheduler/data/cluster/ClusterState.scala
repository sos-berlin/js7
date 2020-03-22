package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.Strings._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, Coupled, CouplingPrepared, FailedOver, PassiveLost, SwitchedOver}
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
        case (ClusterEmpty, BecameSole(activeUri)) =>
          Right(ClusterSole(activeUri))

        case (ClusterSole(primaryUri), BackupNodeAppointed(backupUri)) if primaryUri != backupUri =>
          Right(ClusterNodesAppointed(primaryUri :: backupUri :: Nil))

        //case (ClusterSole(primaryUri), CouplingPrepared(passiveUri)) if primaryUri != passiveUri =>
        //  Right(AwaitingAppointment(primaryUri :: passiveUri :: Nil))

        case (state: ClusterNodesAppointed, CouplingPrepared(passiveUri)) if state.backupUri == passiveUri =>
          Right(ClusterPreparedToBeCoupled(state.uris, active = 0/*primary*/))

        //case (state: AwaitingAppointment, BackupNodeAppointed(backupUri)) if state.backupUri == backupUri =>
        //  Right(ClusterPreparedToBeCoupled(state.uris, active = 0/*primary*/))

        case (state: ClusterPreparedToBeCoupled, Coupled) =>
          Right(ClusterCoupled(state.uris, state.active))

        case (state: ClusterCoupled, SwitchedOver(uri)) if uri == state.passiveUri =>
          Right(ClusterSwitchedOver(state.uris, state.passive))

        case (state: ClusterCoupled, PassiveLost(uri)) if state.passiveUri == uri =>
          Right(ClusterPassiveLost(state.uris, state.active))

        case (state: ClusterCoupled, event: FailedOver)
          if state.activeUri == event.failedActiveUri && state.passiveUri == event.activatedUri =>
          Right(ClusterFailedOver(state.uris, state.passive, event.failedAt))

        case (state: Decoupled, ClusterEvent.CouplingPrepared(passiveUri)) if passiveUri == state.passiveUri =>
          Right(ClusterPreparedToBeCoupled(state.uris, state.active))

        case (_, keyedEvent) => eventNotApplicable(keyedEvent)
      }
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.fromIterable((this != ClusterEmpty) ? ClusterStateSnapshot(this))

  final def isActive(uri: Uri) = maybeActiveUri contains uri

  def isCoupledPassive(uri: Uri) = false
}

object ClusterState
{
  final case class ClusterStateSnapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like ClusterSole but own URI is unknown. Non-permanent state, not stored. */
  case object ClusterEmpty extends ClusterState {
    def maybeActiveUri = None
  }

  sealed trait HasPrimaryNode extends ClusterState {
    /** Contains the primary node URI. */
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
    /** Contains the primary node URI and the backup node URI. */
    def uris: Seq[Uri]
    final def backupUri = uris(1)
    final def passiveUri = uris(passive)
    final def passive = 1 - active

    protected final def nodesString =
      "primary=" + primaryUri.string + ((active == 0) ?: " active") +
        ", backup=" + backupUri.string + ((active == 1) ?: " active")
  }

  /** Sole node of the cluster.
    * Node is active without standby node. */
  final case class ClusterSole(override val primaryUri: Uri)
  extends HasPrimaryNode {
    val uris = primaryUri :: Nil
    def active = 0
  }

  ///** A passive node follows the active node, but it is not yet appointment.
  //  * The URI of the following (passive) node is still unknown. */
  //final case class AwaitingAppointment(uris: Seq[Uri])
  //extends HasBackupNode
  //{
  //  assertIsValid()
  //
  //  def active = 0
  //
  //  override def toString = s"AwaitingAppointment(primary=$primaryUri, backup=$backupUri)"
  //}

  final case class ClusterNodesAppointed(uris: Seq[Uri])
  extends HasBackupNode
  {
    assertIsValid()

    def active = 0
  }

  /** Intermediate state only which is immediately followed by transition ClusterEvent.Coupled -> ClusterCoupled. */
  final case class ClusterPreparedToBeCoupled(uris: Seq[Uri], active: Int)
  extends HasBackupNode
  {
    assertIsValid()

    override def toString = s"ClusterPreparedToBeCoupled($nodesString)"
  }

  sealed trait CoupledOrDecoupled extends HasBackupNode

  /** An active node is coupled with a passive node. */
  final case class ClusterCoupled(uris: Seq[Uri], active: Int)
  extends CoupledOrDecoupled
  {
    assertIsValid()

    override def isCoupledPassive(uri: Uri) = uri == passiveUri

    override def toString = s"ClusterCoupled($nodesString)"
  }

  sealed trait Decoupled extends CoupledOrDecoupled

  final case class ClusterPassiveLost(uris: Seq[Uri], active: Int)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"ClusterPassiveLost($nodesString)"
  }

  final case class ClusterSwitchedOver(uris: Seq[Uri], active: Int)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"ClusterPassiveLost($nodesString)"
  }

  /** Decoupled after failover.
    * @param failedAt the failing nodes journal must be truncated at this point. */
  final case class ClusterFailedOver(uris: Seq[Uri], active: Int, failedAt: JournalPosition)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"ClusterFailedOver($nodesString, $failedAt)"
  }

  implicit val jsonCodec = TypedJsonCodec[ClusterState](
    Subtype(ClusterEmpty),
    Subtype(deriveCodec[ClusterSole]),
    //Subtype(deriveCodec[AwaitingAppointment]),
    Subtype(deriveCodec[ClusterNodesAppointed]),
    Subtype(deriveCodec[ClusterPreparedToBeCoupled]),
    Subtype(deriveCodec[ClusterCoupled]),
    Subtype(deriveCodec[ClusterPassiveLost]),
    Subtype(deriveCodec[ClusterSwitchedOver]),
    Subtype(deriveCodec[ClusterFailedOver]))
}
