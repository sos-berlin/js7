package js7.agent.data

import js7.agent.data.AgentState.AgentMetaState
import js7.agent.data.orderwatch.{FileWatchState, FileWatchStateRecoverer}
import js7.base.crypt.Signed
import js7.base.utils.Collections.implicits.*
import js7.data.agent.AgentRef
import js7.data.cluster.ClusterStateSnapshot
import js7.data.event.{EventCounter, JournalState, SnapshotableStateRecoverer, StandardsRecoverer}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.{SignableItem, SignableItemKey, SignedItemEvent, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItem}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.{Order, OrderId}
import js7.data.state.EngineStateStatistics
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.mutable

final class AgentStateRecoverer
extends SnapshotableStateRecoverer[AgentState], StandardsRecoverer:

  protected val S = AgentState

  private var agentMetaState = AgentMetaState.empty
  private val keyToUnsignedItemState = mutable.Map.empty[UnsignedItemKey, UnsignedItemState]
  private val idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val idToWorkflow = mutable.Map.empty[WorkflowId, Workflow]
  private val fileWatchStateRecoverer = new FileWatchStateRecoverer.Recoverer
  private val pathToJobResource = mutable.Map.empty[JobResourcePath, JobResource]
  private val keyToSignedItem = mutable.Map.empty[SignableItemKey, Signed[SignableItem]]
  private val statistics = EngineStateStatistics.Builder()

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
      fileWatchStateRecoverer.addSnapshot(snapshot)

    case itemState: UnsignedItemState =>
      keyToUnsignedItemState.insert(itemState.item.key, itemState)

    case item: UnsignedSimpleItem =>
      keyToUnsignedItemState.insert(item.path, item.toInitialItemState)

    case o: AgentMetaState =>
      agentMetaState = o

    case o: (JournalState | ClusterStateSnapshot) =>
      addStandardObject(o)

    case o: EngineStateStatistics.SnapshotObjectType =>
      statistics.put(o)


  private def onSignedItemAdded(added: SignedItemEvent.SignedItemAdded): Unit =
    val item = added.signed.value
    keyToSignedItem.insert(item.key, added.signed)
    item match
      case workflow: Workflow =>
        idToWorkflow.insert(workflow.id, workflow.reduceForAgent(agentMetaState.agentPath))
      case jobResource: JobResource =>
        pathToJobResource.insert(jobResource.path, jobResource)
      case _ =>

  def result(): AgentState =
    fixMetaBeforev2_6_3:
      AgentState.empty.copy(
        eventId = eventId,
        standards = standards,
        meta = agentMetaState,
        keyToUnsignedItemState_ =
          (keyToUnsignedItemState.view ++ fileWatchStateRecoverer.result).toMap,
        idToOrder = idToOrder.toMap,
        idToWorkflow = idToWorkflow.toMap,
        pathToJobResource = pathToJobResource.toMap,
        keyToSignedItem = keyToSignedItem.toMap,
        statistics.result())

  private def fixMetaBeforev2_6_3(agentState: AgentState): AgentState =
    var a = agentState
    val meta = a.meta
    if meta != AgentMetaState.empty && meta.directors.isEmpty then
      for agentRef <- a.keyToItem(AgentRef).get(meta.agentPath) do
        a = a.copy(meta = meta.copy(directors = agentRef.directors))
    a
