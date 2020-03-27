package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.Strings._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{Coupled, CouplingPrepared, FailedOver, NodesAppointed, PassiveLost, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterNodeRole.{Backup, Primary}
import com.sos.jobscheduler.data.cluster.ClusterState._
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, JournalPosition, JournaledState, KeyedEvent}
import monix.reactive.Observable
import scala.collection.immutable.Seq

sealed trait ClusterState
extends JournaledState[ClusterState, ClusterEvent]
{
  def roleToMaybeUri(role: ClusterNodeRole): Option[Uri]

  def maybeActiveUri: Option[Uri]

  def applyEvent(keyedEvent: KeyedEvent[ClusterEvent]): Checked[ClusterState] = {
    val event = keyedEvent.event
    if (!keyedEvent.key.isInstanceOf[NoKey])
      eventNotApplicable(keyedEvent)
    else
      (this, event) match {
        case (ClusterEmpty, NodesAppointed(uris)) =>
          Right(ClusterNodesAppointed(uris))

        case (state: ClusterNodesAppointed, CouplingPrepared(passiveUri)) if state.backupUri == passiveUri =>
          Right(ClusterPreparedToBeCoupled(state.uris, active = 0/*primary*/))

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

  def isActive(role: ClusterNodeRole): Boolean

  final def isPassive(role: ClusterNodeRole) = !isActive(role)

  final def isActive(uri: Uri) = maybeActiveUri contains uri

  def isCoupledPassiveRole(role: ClusterNodeRole) = false
}

object ClusterState
{
  final case class ClusterStateSnapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like ClusterSole but own URI is unknown. Non-permanent state, not stored. */
  case object ClusterEmpty extends ClusterState {
    def maybeActiveUri = None

    def roleToMaybeUri(role: ClusterNodeRole) = None

    final def isActive(role: ClusterNodeRole) =
      role match {
        case ClusterNodeRole.Primary => true
        case ClusterNodeRole.Backup => false
      }
  }

  sealed trait HasNodes extends ClusterState
  {
    /** Contains the primary node URI and the backup node URI. */
    def uris: Seq[Uri]
    def active: Int

    final def roleToUri(role: ClusterNodeRole): Uri =
      role match {
        case Primary => uris(0)
        case Backup => uris(1)
      }

    final def passive = 1 - active
    final def primaryUri = uris.head
    final def backupUri = uris(1)
    final def activeUri = uris(active)
    final def passiveUri = uris(passive)
    final def maybeActiveUri = Some(activeUri)
    final def isActive(role: ClusterNodeRole) = role == activeRole

    final def activeRole = active match {
      case 0 => Primary
      case 1 => Backup
    }

    protected final def passiveRole = passive match {
      case 0 => Primary
      case 1 => Backup
    }

    override def roleToMaybeUri(role: ClusterNodeRole) =
      Some(role match {
        case Primary => uris.head
        case Backup =>  uris(1)
      })

    protected def assertIsValid(): Unit = {
      val n = uris.length
      assertThat(n == 2 && uris.toSet.size == n && active >= 0 && active < n)
    }

    protected final def nodesString =
      primaryUri.string + ((active == 0) ?: " active") +
        ", " + backupUri.string + ((active == 1) ?: " active")
  }

  final case class ClusterNodesAppointed(uris: Seq[Uri])
  extends HasNodes
  {
    assertIsValid()

    def active = 0
  }

  /** Intermediate state only, is immediately followed by transition ClusterEvent.Coupled -> ClusterCoupled. */
  final case class ClusterPreparedToBeCoupled(uris: Seq[Uri], active: Int)
  extends HasNodes
  {
    assertIsValid()

    override def toString = s"ClusterPreparedToBeCoupled($nodesString)"
  }

  sealed trait CoupledOrDecoupled extends HasNodes

  /** An active node is coupled with a passive node. */
  final case class ClusterCoupled(uris: Seq[Uri], active: Int)
  extends CoupledOrDecoupled
  {
    assertIsValid()

    override def isCoupledPassiveRole(role: ClusterNodeRole) = role == passiveRole

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
    Subtype(deriveCodec[ClusterNodesAppointed]),
    Subtype(deriveCodec[ClusterPreparedToBeCoupled]),
    Subtype(deriveCodec[ClusterCoupled]),
    Subtype(deriveCodec[ClusterPassiveLost]),
    Subtype(deriveCodec[ClusterSwitchedOver]),
    Subtype(deriveCodec[ClusterFailedOver]))
}
