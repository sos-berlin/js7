package com.sos.jobscheduler.agent

import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, JournalState, JournaledState, KeyedEvent}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderCoreEvent, OrderForked, OrderJoined, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowEvent, WorkflowId}
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final case class AgentState(
  eventId: EventId,
  standards: JournaledState.Standards,
  idToOrder: Map[OrderId, Order[Order.State]],
  idToWorkflow: Map[WorkflowId, Workflow])
extends JournaledState[AgentState]
{
  def toSnapshotObservable =
    journalState.toSnapshotObservable ++
      Observable.fromIterable(idToWorkflow.values) ++
      Observable.fromIterable(idToOrder.values)

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)

  def withStandards(standards: JournaledState.Standards) =
    copy(standards = standards)

  @deprecated
  def applySnapshot(snapshot: Any): Checked[AgentState] =
    snapshot match {
      case o: JournalState =>
        Right(copy(standards = standards.copy(
          journalState = o)))

      case order: Order[Order.State] =>
        Right(copy(
          idToOrder = idToOrder + (order.id -> order)))

      case workflow: Workflow =>
        Right(copy(
          idToWorkflow = idToWorkflow + (workflow.id -> workflow)))
    }

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case KeyedEvent(_: NoKey, WorkflowEvent.WorkflowAttached(workflow)) =>
        // Multiple orders with same Workflow may occur
        // TODO Every Order becomes its own copy of its Workflow? Workflow will never be removed
        Right(copy(
          idToWorkflow = idToWorkflow + (workflow.id -> workflow)))

      case KeyedEvent(_, _: AgentMasterEvent.AgentReadyForMaster) =>
        Right(this)

      case keyedEvent => applyStandardEvent(keyedEvent)
    }

  private def applyOrderEvent(orderId: OrderId, event: OrderEvent) =
    event match {
      case event: OrderEvent.OrderAttached =>
        if (idToOrder.contains(orderId))
          Left(Problem.pure(s"Duplicate order attached: $orderId"))
        else
          Right(copy(
            idToOrder = idToOrder + (orderId -> Order.fromOrderAttached(orderId, event))))

      case OrderEvent.OrderDetached =>
        Right(copy(
          idToOrder = idToOrder - orderId))

      case event: OrderEvent =>
        // See also OrderActor#update
        event match {
          case event: OrderForked =>
            // TODO Check duplicate child OrderIds
            idToOrder.checked(orderId)
              .flatMap(order =>
                Right(copy(
                  idToOrder = idToOrder +
                    (order.id -> order) ++
                    idToOrder(orderId).newForkedOrders(event).map(o => o.id -> o))))

          case _: OrderJoined =>
            idToOrder.checked(orderId)
              .flatMap(_.checkedState[Order.Forked])
              .map(order => copy(
                idToOrder = idToOrder +
                  (order.id -> order) --
                  order.state.childOrderIds))

          case event: OrderCoreEvent =>
            idToOrder.checked(orderId)
              .flatMap(_.update(event))
              .map(order => copy(
                idToOrder = idToOrder + (order.id-> order)))

          case _: OrderStdWritten =>
            // OrderStdWritten is not applied (but forwarded to Master)
            Right(this)
        }
    }
}

object AgentState
{
  val empty = AgentState(EventId.BeforeFirst, JournaledState.Standards.empty, Map.empty, Map.empty)
}
