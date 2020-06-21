package js7.data.cluster

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.ScalazStyle._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterSetting.checkUris
import js7.data.cluster.ClusterSetting.syntax._
import js7.data.cluster.ClusterState._
import js7.data.event.JournaledState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventDrivenState, JournalPosition, KeyedEvent}
import monix.reactive.Observable

sealed trait ClusterState
extends EventDrivenState[ClusterState, ClusterEvent]
{
  def isNonEmptyActive(id: ClusterNodeId): Boolean

  final def applyEvent(keyedEvent: KeyedEvent[ClusterEvent]): Checked[ClusterState] =
    keyedEvent match {
      case KeyedEvent(_: NoKey, event) =>
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

          case (state: Coupled, ClusterActiveNodeShutDown) =>
            Right(CoupledActiveShutDown(state.idToUri, state.activeId))

          case (state: CoupledActiveShutDown, ClusterActiveNodeRestarted) =>
            Right(PassiveLost(state.idToUri, state.activeId))
            // Passive node may recouple now

          case (_, keyedEvent) =>
            Left(EventNotApplicableProblem(keyedEvent, this))
        }

      case _ =>
        Left(EventNotApplicableProblem(keyedEvent, this))
    }

  def toSnapshotObservable =
    Observable.fromIterable((this != Empty) ? ClusterStateSnapshot(this))
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
    this: Product =>

    protected final def assertIsValid(): Unit =
      checkUris(idToUri, activeId).orThrow

    def idToUri: Map[Id, Uri]
    def activeId: Id

    final def isNonEmptyActive(id: Id) = id == activeId
    final def passiveId = idToUri.peerOf(activeId)
    final def passiveUri = idToUri(passiveId)

    protected final def nodesString =
      (for ((id, uri) <- idToUri) yield s"$id${(activeId == id) ?: " is active"}: $uri")
        .mkString(", ")

    override def toString = s"$productPrefix($nodesString)"
  }

  sealed trait CoupledOrDecoupled extends HasNodes {
    this: Product =>
  }

  sealed trait Decoupled extends CoupledOrDecoupled {
    this: Product =>
  }

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
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(idToUri: Map[Id, Uri], activeId: Id)
  extends CoupledOrDecoupled
  {
    assertIsValid()
  }

  /** The active node has shut down while `Coupled` and will continue to be active when restarted.
      The passive node must not fail-over.
      After restart, the active node will stil be active.
    */
  final case class CoupledActiveShutDown(idToUri: Map[Id, Uri], activeId: Id)
  extends Decoupled
  {
    assertIsValid()
  }

  final case class PassiveLost(idToUri: Map[Id, Uri], activeId: Id)
  extends Decoupled
  {
    assertIsValid()
  }

  final case class SwitchedOver(idToUri: Map[Id, Uri], activeId: Id)
  extends Decoupled
  {
    assertIsValid()
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
    Subtype(deriveCodec[CoupledActiveShutDown]),
    Subtype(deriveCodec[PassiveLost]),
    Subtype(deriveCodec[SwitchedOver]),
    Subtype(deriveCodec[FailedOver]))
}
