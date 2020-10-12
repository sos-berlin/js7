package js7.data.cluster

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterSetting.syntax._
import js7.data.cluster.ClusterState._
import js7.data.event.JournaledState.EventNotApplicableProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventDrivenState, JournalPosition, KeyedEvent}
import js7.data.node.NodeId
import monix.reactive.Observable

sealed trait ClusterState
extends EventDrivenState[ClusterState, ClusterEvent]
{
  def isNonEmptyActive(id: NodeId): Boolean

  final def applyEvent(keyedEvent: KeyedEvent[ClusterEvent]): Checked[ClusterState] =
    keyedEvent match {
      case KeyedEvent(_: NoKey, event) =>
        (this, event) match {
          case (Empty, ClusterNodesAppointed(setting)) =>
            Right(NodesAppointed(setting))

          case (state: Decoupled, ClusterCouplingPrepared(activeId)) if state.activeId == activeId =>
            Right(PreparedToBeCoupled(state.setting))

          case (state: PreparedToBeCoupled, ClusterCoupled(activeId)) if state.activeId == activeId =>
            Right(Coupled(state.setting))

          case (state: Coupled, ClusterSwitchedOver(id)) if state.passiveId == id =>
            Right(SwitchedOver(state.setting.copy(activeId = id)))

          case (state: Coupled, ClusterPassiveLost(id)) if state.passiveId == id =>
            Right(PassiveLost(state.setting))

          case (state: Coupled, event: ClusterFailedOver)
            if state.activeId == event.failedActiveId && state.passiveId == event.activatedId =>
            Right(FailedOver(state.setting.copy(activeId = event.activatedId), failedAt = event.failedAt))

          case (state: Coupled, ClusterActiveNodeShutDown) =>
            Right(CoupledActiveShutDown(state.setting))

          case (state: CoupledActiveShutDown, ClusterActiveNodeRestarted) =>
            Right(PassiveLost(state.setting))
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
  private type Id = NodeId


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
      setting.assertIsValid()

    def setting: ClusterSetting
    def idToUri = setting.idToUri
    def activeId = setting.activeId

    final def isNonEmptyActive(id: Id) = id == activeId
    final def passiveId = idToUri.peerOf(activeId)
    final def passiveUri = idToUri(passiveId)

    protected final def nodesString =
      (for ((id, uri) <- idToUri) yield s"$id${(activeId == id) ?? " is active"}: $uri")
        .mkString(", ")

    override def toString = s"$productPrefix($nodesString)"
  }

  sealed trait CoupledOrDecoupled extends HasNodes {
    this: Product =>
  }

  sealed trait Decoupled extends CoupledOrDecoupled {
    this: Product =>
  }

  final case class NodesAppointed(setting: ClusterSetting)
  extends Decoupled
  {
    assertIsValid()
  }

  /** Intermediate state only, is immediately followed by transition ClusterEvent.Coupled -> Coupled. */
  final case class PreparedToBeCoupled(setting: ClusterSetting)
  extends HasNodes
  {
    assertIsValid()
  }

  /** An active node is coupled with a passive node. */
  final case class Coupled(setting: ClusterSetting)
  extends CoupledOrDecoupled
  {
    assertIsValid()
  }

  /** The active node has shut down while `Coupled` and will continue to be active when restarted.
      The passive node must not fail-over.
      After restart, the active node will stil be active.
    */
  final case class CoupledActiveShutDown(setting: ClusterSetting)
  extends Decoupled  // CoupledActiveShutDown is Decoupled ???
  {
    assertIsValid()
  }

  final case class PassiveLost(setting: ClusterSetting)
  extends Decoupled
  {
    assertIsValid()
  }

  final case class SwitchedOver(setting: ClusterSetting)
  extends Decoupled
  {
    assertIsValid()
  }

  /** Decoupled after failover.
    * @param failedAt the failing nodes journal must be truncated at this point. */
  final case class FailedOver(setting: ClusterSetting, failedAt: JournalPosition)
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
