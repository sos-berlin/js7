package js7.agent.data

import js7.agent.data.AgentState.AgentMetaState
import js7.agent.data.orderwatch.{AllFileWatchesState, FileWatchState}
import js7.base.problem.Checked._
import js7.base.utils.Collections.implicits._
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.cluster.ClusterState
import js7.data.event.{EventId, JournalState, JournaledState, JournaledStateBuilder, Stamped}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.mutable

final class AgentStateBuilder
extends JournaledStateBuilder[AgentState]
{
  protected val S = AgentState

  private var _journalState = JournalState.empty
  private var _eventId = EventId.BeforeFirst
  private var agentMetaState = AgentMetaState.empty
  private val idToOrder = mutable.Map.empty[OrderId, Order[Order.State]]
  private val idToWorkflow = mutable.Map.empty[WorkflowId, Workflow]
  private val pathToCalendar = mutable.Map.empty[CalendarPath, Calendar]
  private val allFileWatchesState = new AllFileWatchesState.Builder
  private val pathToJobResource = mutable.Map.empty[JobResourcePath, JobResource]
  private var _state = AgentState.empty

  protected def onInitializeState(state: AgentState) =
    _state = state

  protected def onAddSnapshotObject = {
    case order: Order[Order.State] =>
      idToOrder.insert(order.id -> order)

    case workflow: Workflow =>
      idToWorkflow.insert(workflow.id -> workflow)

    case jobResource: JobResource =>
      pathToJobResource.insert(jobResource.path -> jobResource)

    case calendar: Calendar =>
      pathToCalendar.insert(calendar.path -> calendar)

    case snapshot: FileWatchState.Snapshot =>
      allFileWatchesState.addSnapshot(snapshot)

    case o: JournalState =>
      _journalState = o

    case o: AgentMetaState =>
      agentMetaState = o
  }

  override protected def onOnAllSnapshotsAdded() = {
    _state = AgentState(
      _eventId,
      JournaledState.Standards(_journalState, ClusterState.Empty),
      agentMetaState,
      idToOrder.toMap,
      idToWorkflow.toMap,
      allFileWatchesState.result(),
      pathToJobResource.toMap,
      pathToCalendar.toMap)
  }

  protected def onAddEvent = {
    case Stamped(eventId, _, keyedEvent) =>
      _eventId = eventId
      _state = _state.applyEvent(keyedEvent).orThrow
  }

  def result() = _state.copy(eventId = _eventId)

  def journalState = JournalState.empty

  def clusterState = ClusterState.Empty
}
