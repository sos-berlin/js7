package js7.data.cluster

import fs2.{Pure, Stream}
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.compilable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterResetStarted, ClusterSettingUpdated, ClusterSwitchedOver, ClusterWatchRegistered}
import js7.data.cluster.ClusterSetting.syntax.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventDriven, EventDrivenState, JournalPosition, KeyedEvent}
import js7.data.node.NodeId
import scala.reflect.ClassTag

sealed trait ClusterState
extends EventDriven[ClusterState, ClusterEvent]
with EventDrivenState[ClusterState, ClusterEvent]:

  import ClusterState.*

  def companion: ClusterState.type =
    ClusterState

  def asCode: AsCode

  final def isActive(nodeId: NodeId, isBackup: Boolean): Boolean =
    this == Empty && !isBackup || isNonEmptyActive(nodeId)

  def isNonEmptyActive(id: NodeId): Boolean

  def isEmptyOrActive(id: NodeId): Boolean

  def ifHasNodes: Option[HasNodes] =
    this match
      case hasNodes: HasNodes => Some(hasNodes)
      case _ => None

  final def applyKeyedEvent(keyedEvent: KeyedEvent[ClusterEvent]): Checked[ClusterState] =
    compilable(keyedEvent.key: NoKey)
    applyEvent(keyedEvent.event)

  final def applyEvent(event: ClusterEvent): Checked[ClusterState] =
    (this, event) match
      case (Empty, ClusterNodesAppointed(setting)) =>
        Right(NodesAppointed(setting))

      case (state: HasNodes/*IsCoupledOrDecoupled?*/, ClusterSettingUpdated(maybePassiveUri)) =>
        Right(
          maybePassiveUri.fold(state)(uri =>
            state.withSetting(state.setting.withPassiveUri(uri))))

      case (state: IsDecoupled, ClusterCouplingPrepared(activeId)) if state.activeId == activeId =>
        Right(PreparedToBeCoupled(state.setting))

      case (state: PreparedToBeCoupled, ClusterCoupled(activeId)) if state.activeId == activeId =>
        Right(Coupled(state.setting))

      case (state: Coupled, ClusterPassiveLost(id)) if state.passiveId == id =>
        // Be sure that any event on Coupled leaves this state !!!
        // In case of a passive heartbeat loss, the JournalActor gets a JournalActor.Input.PassiveLost,
        // event if a concurrent ClusterEvent changes the ClusterState.
        // No ClusterEvent should go from Coupled to Coupled.
        Right(PassiveLost(state.setting))

      case (state: Coupled, ClusterSwitchedOver(id)) if state.passiveId == id =>
        Right(SwitchedOver(state.setting.copy(activeId = id)))

      case (state: Coupled, event: ClusterFailedOver)
        if state.activeId == event.failedActiveId && state.passiveId == event.activatedId =>
        Right(FailedOver(state.setting.copy(activeId = event.activatedId), failedAt = event.failedAt))

      case (state: Coupled, ClusterActiveNodeShutDown) =>
        Right(ActiveShutDown(state.setting))

      case (state: ActiveShutDown, ClusterActiveNodeRestarted) =>
        Right(PassiveLost(state.setting))
        // Passive node may recouple now

      case (state: HasNodes, ClusterWatchRegistered(clusterWatchId)) =>
        Right(state.withSetting(setting = state.setting.copy(
          clusterWatchId = Some(clusterWatchId))))

      case (_: Coupled, ClusterResetStarted) =>
        Right(this) // Do we want an own state ???

      case (_, keyedEvent) =>
        eventNotApplicable(keyedEvent)

  def estimatedSnapshotSize: Int =
    if this != Empty then 1 else 0

  def toSnapshotStream: Stream[Pure, ClusterStateSnapshot] =
    Stream.iterable((this != Empty) ? ClusterStateSnapshot(this))

  def toShortString: String =
    this match
      case Empty => "ClusterState:Empty"
      case hasNodes: HasNodes =>
        s"${hasNodes.getClass.simpleScalaName}(${hasNodes.activeId} is active)"


object ClusterState
extends EventDriven.Companion[ClusterState, ClusterEvent]
with EventDrivenState.Companion[ClusterState]:

  private type Id = NodeId

  override val name: String = getClass.shortClassName

  /** Cluster has not been initialized.
    * Like ClusterSole but own URI is unknown. Non-permanent state, not stored. */
  case object Empty extends ClusterState:
    def asCode = AsCode.Empty
    def isNonEmptyActive(id: Id) = false
    def isEmptyOrActive(id: Id) = true

  sealed trait HasNodes extends ClusterState:
    this: Product =>

    val setting: ClusterSetting
    def idToUri: Map[Id, Uri] = setting.idToUri
    def activeId: NodeId = setting.activeId
    def timing: ClusterTiming = setting.timing

    def withSetting(setting: ClusterSetting): HasNodes

    final def isNonEmptyActive(id: Id): Boolean = id == activeId
    final def isEmptyOrActive(id: Id): Boolean = id == activeId
    final def activeUri: Uri = idToUri(activeId)
    final def passiveId: NodeId = idToUri.peerOf(activeId)
    final def passiveUri: Uri = idToUri(passiveId)
    final def peerOf(nodeId: NodeId): NodeId = idToUri.peerOf(nodeId)

    protected final def nodesString =
      idToUri
        .map { case (id, uri) =>
          s"${if activeId == id then "active" else "passive"} ${id.string} $uri"
        }
        .mkString(", ")

    override def toString =
      s"$productPrefix($nodesString${setting.clusterWatchId.fold("")(o => ", " + o)})"
  object HasNodes:
    def unapply(clusterState: ClusterState.HasNodes): Some[ClusterSetting] =
      Some(clusterState.setting)

    implicit val jsonCodec: TypedJsonCodec[HasNodes] = TypedJsonCodec(
      Subtype(deriveCodec[NodesAppointed]),
      Subtype(deriveCodec[PreparedToBeCoupled]),
      Subtype(deriveCodec[Coupled]),
      Subtype(deriveCodec[ActiveShutDown]),
      Subtype(deriveCodec[PassiveLost]),
      Subtype(deriveCodec[SwitchedOver]),
      Subtype(deriveCodec[FailedOver]))

  sealed trait IsCoupledOrDecoupled extends HasNodes:
    this: Product =>

    def withSetting(setting: ClusterSetting): IsCoupledOrDecoupled

  sealed trait IsDecoupled extends IsCoupledOrDecoupled:
    this: Product =>

  /** Initial appointment of the nodes. */
  final case class NodesAppointed(setting: ClusterSetting)
  extends IsDecoupled:
    def asCode = AsCode.NodesAppointed

    def withSetting(setting: ClusterSetting): NodesAppointed =
      copy(setting = setting)

  /** Intermediate state only, is immediately followed by transition ClusterCoupled -> Coupled. */
  final case class PreparedToBeCoupled(setting: ClusterSetting)
  extends HasNodes:
    def asCode = AsCode.PreparedToBeCoupled

    def withSetting(setting: ClusterSetting): HasNodes = copy(setting = setting)

  /** An active node is coupled with a passive node. */
  final case class Coupled(setting: ClusterSetting)
  extends IsCoupledOrDecoupled:
    def asCode = AsCode.Coupled

    def withSetting(setting: ClusterSetting): Coupled =
      copy(setting = setting)

  /** The active node has shut down while `Coupled` and will continue to be active when restarted.
      The passive node must not fail-over.
      After restart, the active node will be still active.
    */
  final case class ActiveShutDown(setting: ClusterSetting)
  extends IsDecoupled:
    def asCode = AsCode.ActiveShutDown

    def withSetting(setting: ClusterSetting): ActiveShutDown =
      copy(setting = setting)

  final case class SwitchedOver(setting: ClusterSetting)
  extends IsDecoupled:
    def asCode = AsCode.SwitchedOver

    def withSetting(setting: ClusterSetting): SwitchedOver =
      copy(setting = setting)

  sealed trait IsNodeLost extends IsDecoupled:
    this: Product =>

  final case class PassiveLost(setting: ClusterSetting)
  extends IsNodeLost:
    def asCode = AsCode.PassiveLost

    def withSetting(setting: ClusterSetting): PassiveLost =
      copy(setting = setting)

  /** Decoupled after failover.
    * @param failedAt the failing node's journal must be truncated at this point. */
  final case class FailedOver(setting: ClusterSetting, failedAt: JournalPosition)
  extends IsNodeLost:
    def asCode = AsCode.FailedOver

    def withSetting(setting: ClusterSetting): FailedOver =
      copy(setting = setting)

    override def toShortString =
      s"FailedOver(${setting.passiveId.string} --> ${setting.activeId.string})"

    override def toString = "FailedOver(" +
      s"${setting.passiveId.string} --> ${setting.activeId.string} at $failedAt)"

  implicit val jsonCodec: TypedJsonCodec[ClusterState] = TypedJsonCodec(
    Subtype(Empty),
    Subtype[HasNodes])


  /** Experimental for Prometheus metrics.
    * <p>Prometheus supports only numeric values. */
  enum AsCode(val index: Int):
    case Empty extends AsCode(0)
    case NodesAppointed extends AsCode(1)
    case PreparedToBeCoupled extends AsCode(2)
    case Coupled extends AsCode(3)
    case ActiveShutDown extends AsCode(4)
    case PassiveLost extends AsCode(5)
    case SwitchedOver extends AsCode(6)
    case FailedOver extends AsCode(7)
