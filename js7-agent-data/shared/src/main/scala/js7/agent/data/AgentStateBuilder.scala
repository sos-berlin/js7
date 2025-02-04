package js7.agent.data

import js7.agent.data.AgentState.AgentMetaState
import js7.agent.data.orderwatch.{FileWatchState, FileWatchStateHandler}
import js7.base.crypt.Signed
import js7.base.problem.Checked.*
import js7.base.utils.Collections.implicits.*
import js7.data.agent.AgentRef
import js7.data.cluster.{ClusterState, ClusterStateSnapshot}
import js7.data.event.{JournalState, SnapshotableStateBuilder, Stamped}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.{SignableItem, SignableItemKey, SignedItemEvent, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItem}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.mutable

final class AgentStateBuilder
extends SnapshotableStateBuilder[AgentState]:

  protected val S = AgentState

  private var agentMetaState = AgentMetaState.empty
  private val keyToUnsignedItemState = mutable.Map.empty[UnsignedItemKey, UnsignedItemState]
  private val idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val idToWorkflow = mutable.Map.empty[WorkflowId, Workflow]
  private val fileWatchStateBuilder = new FileWatchStateHandler.Builder
  private val pathToJobResource = mutable.Map.empty[JobResourcePath, JobResource]
  private val keyToSignedItem = mutable.Map.empty[SignableItemKey, Signed[SignableItem]]
  private var _state = AgentState.empty

  protected def onInitializeState(state: AgentState): Unit =
    _state = state

  protected def onAddSnapshotObject =
    case order: Order[Order.State] =>
      idToOrder.insert(order.id, order)

    case signedItemAdded: SignedItemAdded =>
      onSignedItemAdded(signedItemAdded)

    case workflow: Workflow =>
      // COMPATIBLE with v2.1
      idToWorkflow.insert(workflow.id, workflow)

    case jobResource: JobResource =>
      // COMPATIBLE with v2.1
      pathToJobResource.insert(jobResource.path, jobResource)

    case snapshot: FileWatchState.Snapshot =>
      fileWatchStateBuilder.addSnapshot(snapshot)

    case itemState: UnsignedItemState =>
      keyToUnsignedItemState.insert(itemState.item.key, itemState)

    case item: UnsignedSimpleItem =>
      keyToUnsignedItemState.insert(item.path, item.toInitialItemState)

    case o: AgentMetaState =>
      agentMetaState = o

    case journalState: JournalState =>
      _state = _state.copy(
        standards = _state.standards.copy(
          journalState = journalState))

    case ClusterStateSnapshot(clusterState) =>
      _state = _state.copy(
        standards = _state.standards.copy(
          clusterState = clusterState))

  private def onSignedItemAdded(added: SignedItemEvent.SignedItemAdded): Unit =
    val item = added.signed.value
    keyToSignedItem.insert(item.key, added.signed)
    item match
      case workflow: Workflow =>
        idToWorkflow.insert(workflow.id, workflow.reduceForAgent(agentMetaState.agentPath))
      case jobResource: JobResource =>
        pathToJobResource.insert(jobResource.path, jobResource)
      case _ =>

  override protected def onOnAllSnapshotsObjectsAdded(): Unit =
    _state = _state.copy(
      eventId = eventId,
      meta = agentMetaState,
      keyToUnsignedItemState_ = (keyToUnsignedItemState.view ++ fileWatchStateBuilder.result).toMap,
      idToOrder = idToOrder.toMap,
      idToWorkflow = idToWorkflow.toMap,
      pathToJobResource = pathToJobResource.toMap,
      keyToSignedItem = keyToSignedItem.toMap)

  protected def onAddEvent =
    case Stamped(eventId, _, keyedEvent) =>
      _state = _state.applyKeyedEvent(keyedEvent).orThrow

  def journalState: JournalState =
    _state.journalState

  def clusterState: ClusterState =
    _state.clusterState

  def result(): AgentState =
    fixMetaBeforev2_6_3()
    _state.copy(eventId = eventId)

  private def fixMetaBeforev2_6_3(): Unit =
    val meta = _state.meta
    if meta != AgentMetaState.empty && meta.directors.isEmpty then
      for agentRef <- _state.keyToItem(AgentRef).get(meta.agentPath) do
        _state = _state.copy(meta = meta.copy(directors = agentRef.directors))
