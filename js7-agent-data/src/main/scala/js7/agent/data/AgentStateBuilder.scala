package js7.agent.data

import js7.base.problem.Checked._
import js7.base.utils.Collections.implicits._
import js7.data.cluster.ClusterState
import js7.data.event.{EventId, JournalState, JournaledState, JournaledStateBuilder, Stamped}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.mutable

final class AgentStateBuilder
extends JournaledStateBuilder[AgentState]
{
  private var _journalState = JournalState.empty
  private var _eventId = EventId.BeforeFirst
  private var idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private var idToWorkflow = mutable.Map.empty[WorkflowId, Workflow]
  private var _state = AgentState.empty

  protected def onInitializeState(state: AgentState) = {
    _eventId = state.eventId
    idToOrder = mutable.Map() ++ state.idToOrder
    idToWorkflow = mutable.Map() ++ state.idToWorkflow
  }

  protected def onAddSnapshotObject = {
    case o: JournalState =>
      _journalState = o

    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case workflow: Workflow =>
      idToWorkflow.insert(workflow.id -> workflow)
  }

  protected def onOnAllSnapshotsAdded() = {
    _state = AgentState(
      _eventId,
      JournaledState.Standards(_journalState, ClusterState.Empty),
      idToOrder.toMap,
      idToWorkflow.toMap)
  }

  protected def onAddEvent = {
    case Stamped(eventId, _, keyedEvent) =>
      _eventId = eventId
      _state = state.applyEvent(keyedEvent).orThrow
  }

  def state = _state.copy(eventId = _eventId)

  def journalState = JournalState.empty

  def clusterState = ClusterState.Empty
}
