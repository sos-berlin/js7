package js7.data.subagent

import js7.data.event.JournalEvent.Heartbeat
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, ItemContainer, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec}
import js7.data.item.{InventoryItem, InventoryItemKey}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten, OrderStderrWritten, OrderStdoutWritten}
import js7.data.subagent.SubagentEvent.{SubagentItemAttached, SubagentShutdown}
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.{MapView, View}

final case class SubagentState(
  eventId: EventId,
  idToWorkflow: Map[WorkflowId, Workflow],
  pathToJobResource: Map[JobResourcePath, JobResource])
extends SubagentDriverState[SubagentState]
with ItemContainer
{
  val companion: SubagentState.type = SubagentState

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match {
      case KeyedEvent(_: NoKey, SubagentItemAttached(workflow: Workflow)) =>
        Right(copy(
          idToWorkflow = idToWorkflow + (workflow.id -> workflow)))

      case KeyedEvent(_: NoKey, SubagentItemAttached(jobResource: JobResource)) =>
        Right(copy(
          pathToJobResource = pathToJobResource + (jobResource.path -> jobResource)))

      case KeyedEvent(_, _: OrderProcessed | _: OrderStdWritten | _: SubagentShutdown) =>
        Right(this)

      case _ => eventNotApplicable(keyedEvent)
    }

  def keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey,InventoryItem] {
      def get(key: InventoryItemKey) =
        key match {
          case WorkflowId.as(id) => idToWorkflow.get(id)
          case path: JobResourcePath => pathToJobResource.get(path)
          case _ => None
        }

      def iterator =
        items.iterator.map(o => o.key -> o)
    }

  lazy val items: View[InventoryItem] =
    idToWorkflow.values.view ++
      pathToJobResource.values.view
}

object SubagentState
extends JournaledState.Companion[SubagentState]
with ItemContainer.Companion[SubagentState]
{
  val empty = SubagentState(EventId.BeforeFirst, Map.empty, Map.empty)

  protected def inventoryItems =
    Seq(Workflow, JobResource)

  implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec(
      KeyedSubtype[SubagentEvent],
      KeyedSubtype.singleEvent[OrderStdoutWritten],
      KeyedSubtype.singleEvent[OrderStderrWritten],
      KeyedSubtype.singleEvent[OrderProcessed],
      KeyedSubtype.singleEvent(Heartbeat))
}
