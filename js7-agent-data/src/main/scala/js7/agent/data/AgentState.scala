package js7.agent.data

import js7.agent.data.event.AgentControllerEvent
import js7.agent.data.ordersource.{AllFileOrderSourcesState, FileOrderSourceState}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import js7.data.event.{Event, EventId, JournalEvent, JournalState, JournaledState, KeyedEvent, KeyedEventTypedJsonCodec}
import js7.data.item.SimpleItemEvent.SimpleItemAttachedToAgent
import js7.data.item.{SimpleItem, SimpleItemEvent}
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderForked, OrderJoined, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.ordersource.{FileOrderSource, OrderSourceEvent, OrderSourceId}
import js7.data.workflow.{Workflow, WorkflowEvent, WorkflowId}
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class AgentState(
  eventId: EventId,
  standards: JournaledState.Standards,
  idToOrder: Map[OrderId, Order[Order.State]],
  idToWorkflow: Map[WorkflowId, Workflow],
  allFileOrderSourcesState: AllFileOrderSourcesState)
extends JournaledState[AgentState]
{
  def estimatedSnapshotSize =
    standards.snapshotSize +
      idToWorkflow.size +
      idToOrder.size +
      allFileOrderSourcesState.estimatedSnapshotSize

  def toSnapshotObservable =
    standards.toSnapshotObservable ++
      Observable.fromIterable(idToWorkflow.values) ++
      Observable.fromIterable(idToOrder.values) ++
      allFileOrderSourcesState.toSnapshot

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case KeyedEvent(_: NoKey, WorkflowEvent.WorkflowAttached(workflow)) =>
        // Multiple orders with same Workflow may occur
        // TODO Every Order becomes its own copy of its Workflow? Workflow will never be removed
        Right(reuseIfEqual(this, copy(
          idToWorkflow = idToWorkflow + (workflow.id -> workflow))))

      case KeyedEvent(_, _: AgentControllerEvent.AgentReadyForController) =>
        Right(this)

      case KeyedEvent(orderSourceId: OrderSourceId, event: OrderSourceEvent) =>
        allFileOrderSourcesState.applyEvent(orderSourceId <-: event)
          .map(o => copy(allFileOrderSourcesState = o))

      case KeyedEvent(_: NoKey, SimpleItemAttachedToAgent(fos: FileOrderSource)) =>
        Right(copy(
          allFileOrderSourcesState = allFileOrderSourcesState.attach(fos)))

      case keyedEvent => applyStandardEvent(keyedEvent)
    }

  private def applyOrderEvent(orderId: OrderId, event: OrderEvent) =
    event match {
      case event: OrderEvent.OrderAttachedToAgent =>
        if (idToOrder.contains(orderId))
          Left(Problem.pure(s"Duplicate order attached: $orderId"))
        else
          Right(copy(
            idToOrder = idToOrder + (orderId -> Order.fromOrderAttached(orderId, event))))

      case OrderEvent.OrderDetached =>
        Right(copy(
          idToOrder = idToOrder - orderId))

      case event: OrderCoreEvent =>
        // See also OrderActor#update
        idToOrder.checked(orderId)
          .flatMap(_.applyEvent(event))
          .flatMap(order =>
            event match {
              case event: OrderForked =>
                // TODO Check duplicate child OrderIds
                Right(copy(
                  idToOrder = idToOrder +
                    (order.id -> order) ++
                    idToOrder(orderId).newForkedOrders(event).map(o => o.id -> o)))

              case _: OrderJoined =>
                //order.checkedState[Order.Forked]
                //  .map(order => copy(
                //    idToOrder = idToOrder +
                //      (order.id -> order) --
                //      order.state.childOrderIds))
                Left(Problem.pure("OrderJoined not applicable on AgentState"))

              case _: OrderCoreEvent =>
                Right(copy(
                  idToOrder = idToOrder + (order.id -> order)))
            })

      case _: OrderStdWritten =>
        // OrderStdWritten is not applied (but forwarded to Controller)
        Right(this)
    }
}

object AgentState extends JournaledState.Companion[AgentState]
{
  val empty = AgentState(EventId.BeforeFirst, JournaledState.Standards.empty, Map.empty, Map.empty,
    AllFileOrderSourcesState.empty)

  private val simpleItemCompanions = Seq[SimpleItem.Companion](
    FileOrderSource)

  implicit val simpleItemJsonCodec: TypedJsonCodec[SimpleItem] =
    TypedJsonCodec(simpleItemCompanions.map(_.subtype): _*)

  implicit val simpleItemEventJsonCodec =
    SimpleItemEvent.jsonCodec(simpleItemCompanions)

  override implicit val snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    TypedJsonCodec[Any](
      Subtype[JournalState],
      Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder),
      Subtype[Order[Order.State]],
      Subtype[FileOrderSourceState.Snapshot])

  //import generic.orderSourceIdJsonCodec
  override implicit val keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event] =
    KeyedEventTypedJsonCodec[Event](
      KeyedSubtype[JournalEvent],
      KeyedSubtype[OrderEvent],
      KeyedSubtype.singleEvent[WorkflowEvent.WorkflowAttached],
      KeyedSubtype[AgentControllerEvent],
      KeyedSubtype[SimpleItemEvent],
      KeyedSubtype[OrderSourceEvent])

  def newBuilder() = new AgentStateBuilder
}
