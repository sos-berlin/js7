package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.Strings._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{Coupled, CouplingPrepared, FailedOver, NodesAppointed, PassiveLost, SwitchedOver}
import com.sos.jobscheduler.data.cluster.ClusterSetting.checkUris
import com.sos.jobscheduler.data.cluster.ClusterSetting.syntax._
import com.sos.jobscheduler.data.cluster.ClusterState._
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, JournalPosition, JournaledState, KeyedEvent}
import monix.reactive.Observable

sealed trait ClusterState
extends JournaledState[ClusterState, ClusterEvent]
{
  def isNonEmptyActive(id: ClusterNodeId): Boolean

  def applyEvent(keyedEvent: KeyedEvent[ClusterEvent]): Checked[ClusterState] = {
    val event = keyedEvent.event
    if (!keyedEvent.key.isInstanceOf[NoKey])
      eventNotApplicable(keyedEvent)
    else
      (this, event) match {
        case (ClusterEmpty, NodesAppointed(idToUris, activeId)) =>
          Right(ClusterNodesAppointed(idToUris, activeId))

        case (state: ClusterNodesAppointed, CouplingPrepared(activeId)) if state.activeId == activeId =>
          Right(ClusterPreparedToBeCoupled(state.idToUri, activeId = state.activeId))

        case (state: ClusterPreparedToBeCoupled, Coupled(activeId)) if state.activeId == activeId =>
          Right(ClusterCoupled(state.idToUri, state.activeId))

        case (state: ClusterCoupled, SwitchedOver(id)) if state.passiveId == id =>
          Right(ClusterSwitchedOver(state.idToUri, state.passiveId))

        case (state: ClusterCoupled, PassiveLost(id)) if state.passiveId == id =>
          Right(ClusterPassiveLost(state.idToUri, state.activeId))

        case (state: ClusterCoupled, event: FailedOver)
          if state.activeId == event.failedActiveId && state.passiveId == event.activatedId =>
          Right(ClusterFailedOver(state.idToUri, event.activatedId, event.failedAt))

        case (state: Decoupled, ClusterEvent.CouplingPrepared(activeId)) if state.activeId == activeId =>
          Right(ClusterPreparedToBeCoupled(state.idToUri, state.activeId))

        case (_, keyedEvent) => eventNotApplicable(keyedEvent)
      }
  }

  def withEventId(eventId: EventId) =
    this  // EventId brauchen wir nicht ???

  def toSnapshotObservable =
    Observable.fromIterable((this != ClusterEmpty) ? ClusterStateSnapshot(this))

  def isCoupledPassiveRole(id: ClusterNodeId) = false
}

object ClusterState
{
  private type Id = ClusterNodeId

  final case class ClusterStateSnapshot(clusterState: ClusterState)

  /** Cluster has not been initialized.
    * Like ClusterSole but own URI is unknown. Non-permanent state, not stored. */
  case object ClusterEmpty extends ClusterState
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
      (for ((id, uri) <- idToUri) yield s"$id=$uri${(activeId == id) ?: " active"}")
        .mkString(", ")
  }

  final case class ClusterNodesAppointed(idToUri: Map[Id, Uri], activeId: Id)
  extends HasNodes
  {
    assertIsValid()
  }

  /** Intermediate state only, is immediately followed by transition ClusterEvent.Coupled -> ClusterCoupled. */
  final case class ClusterPreparedToBeCoupled(idToUri: Map[Id, Uri], activeId: Id)
  extends HasNodes
  {
    assertIsValid()

    override def toString = s"ClusterPreparedToBeCoupled($nodesString)"
  }

  sealed trait CoupledOrDecoupled extends HasNodes

  /** An active node is coupled with a passive node. */
  final case class ClusterCoupled(idToUri: Map[Id, Uri], activeId: Id)
  extends CoupledOrDecoupled
  {
    assertIsValid()

    override def isCoupledPassiveRole(id: Id) = id == passiveId

    override def toString = s"ClusterCoupled($nodesString)"
  }

  sealed trait Decoupled extends CoupledOrDecoupled

  final case class ClusterPassiveLost(idToUri: Map[Id, Uri], activeId: Id)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"ClusterPassiveLost($nodesString)"
  }

  final case class ClusterSwitchedOver(idToUri: Map[Id, Uri], activeId: Id)
  extends Decoupled
  {
    assertIsValid()

    override def toString = s"ClusterPassiveLost($nodesString)"
  }

  /** Decoupled after failover.
    * @param failedAt the failing nodes journal must be truncated at this point. */
  final case class ClusterFailedOver(idToUri: Map[Id, Uri], activeId: Id, failedAt: JournalPosition)
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
