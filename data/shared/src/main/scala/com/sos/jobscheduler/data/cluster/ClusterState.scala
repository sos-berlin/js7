package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.Strings._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterSetting.checkUris
import com.sos.jobscheduler.data.cluster.ClusterSetting.syntax._
import com.sos.jobscheduler.data.cluster.ClusterState._
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, JournalPosition, JournaledState, KeyedEvent}
import monix.reactive.Observable

sealed trait ClusterState
extends JournaledState[ClusterState, ClusterEvent]
{
  def isNonEmptyActive(id: ClusterNodeId): Boolean

  // ClusterState should be included in MasterState, which provides journalState ???
  def journalState = throw new NotImplementedError
  def clusterState = this

  def applyEvent(keyedEvent: KeyedEvent[ClusterEvent]): Checked[ClusterState] = {
    val event = keyedEvent.event
    if (!keyedEvent.key.isInstanceOf[NoKey])
      eventNotApplicable(keyedEvent)
    else
      (this, event) match {
        case (Empty, ClusterNodesAppointed(idToUris, activeId)) =>
          Right(NodesAppointed(idToUris, activeId))

        case (state: Decoupled, ClusterCouplingPrepared(activeId)) if state.activeId == activeId =>
          Right(PreparedToBeCoupled(state.idToUri, state.activeId))

        case (state: PreparedToBeCoupled, ClusterCoupled(activeId)) if state.activeId == activeId =>
          Right(Coupled(state.idToUri, state.activeId))

        case (state: Coupled, ClusterSwitchedOver(id)) if state.passiveId == id =>
          Right(SwitchedOver(state.idToUri, state.passiveId))

        case (state: Coupled, ClusterPassiveLost(id)) if state.passiveId == id =>
          Right(PassiveLost(state.idToUri, state.activeId))

        case (state: Coupled, event: ClusterFailedOver)
          if state.activeId == event.failedActiveId && state.passiveId == event.activatedId =>
          Right(FailedOver(state.idToUri, event.activatedId, event.failedAt))


        case (_, keyedEvent) => eventNotApplicable(keyedEvent)
      }
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.fromIterable((this != Empty) ? ClusterStateSnapshot(this))

  def isCoupledPassiveRole(id: ClusterNodeId) = false
}

object ClusterState
{
  private type Id = ClusterNodeId

  final case class ClusterStateSnapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like ClusterSole but own URI is unknown. Non-permanent state, not stored. */
  case object Empty extends ClusterState
  {
    def isNonEmptyActive(id: Id) = false
  }

  sealed trait HasNodes extends ClusterState
  {
    protected final def assertIsValid(): Unit =
      checkUris(idToUri, activeId).orThrow

    def idToUri: Map[Id, Uri]
    def activeId: Id

    final def isNonEmptyActive(id: Id) = id == activeId
    final def passiveId = idToUri.peerOf(activeId)
    final def passiveUri = idToUri(passiveId)

    protected final def nodesString =
      (for ((id, uri) <- idToUri) yield s"$id ($uri)${(activeId == id) ?: " active"}")
        .mkString(", ")
  }

  sealed trait CoupledOrDecoupled extends HasNodes

  sealed trait Decoupled extends CoupledOrDecoupled

  final case class NodesAppointed(idToUri: Map[Id, Uri], activeId: Id)
  extends Decoupled
  {
    assertIsValid()
  }

  /** Intermediate state only, is immediately followed by transition ClusterEvent.Coupled -> Coupled. */
  final case class PreparedToBeCoupled(idToUri: Map[Id, Uri], activeId: Id)
  extends HasNodes
  {
    assertIsValid()

    override def toString = s"PreparedToBeCoupled($nodesString)"
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(idToUri: Map[Id, Uri], activeId: Id)
  extends CoupledOrDecoupled
  {
    assertIsValid()

    override def isCoupledPassiveRole(id: Id) = id == passiveId

    override def toString = s"Coupled($nodesString)"
  }

  final case class PassiveLost(idToUri: Map[Id, Uri], activeId: Id)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"PassiveLost($nodesString)"
  }

  final case class SwitchedOver(idToUri: Map[Id, Uri], activeId: Id)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"PassiveLost($nodesString)"
  }

  /** Decoupled after failover.
    * @param failedAt the failing nodes journal must be truncated at this point. */
  final case class FailedOver(idToUri: Map[Id, Uri], activeId: Id, failedAt: JournalPosition)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"FailedOver($nodesString, $failedAt)"
  }

  implicit val jsonCodec = TypedJsonCodec[ClusterState](
    Subtype(Empty),
    Subtype(deriveCodec[NodesAppointed]),
    Subtype(deriveCodec[PreparedToBeCoupled]),
    Subtype(deriveCodec[Coupled]),
    Subtype(deriveCodec[PassiveLost]),
    Subtype(deriveCodec[SwitchedOver]),
    Subtype(deriveCodec[FailedOver]))
}
