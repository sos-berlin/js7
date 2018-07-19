package com.sos.jobscheduler.master.web.master.api.fatevent

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.Agent
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import com.sos.jobscheduler.data.order.OrderFatEvent.{OrderAddedFat, OrderFinishedFat, OrderForkedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdWrittenFat}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderFatEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Position, Workflow}

/**
  * @author Joacim Zschimmer
  */
private[fatevent] final case class FatState(eventId: EventId, repo: Repo, idToOrder: Map[OrderId, Order[Order.State]])
{
  def toFatOrderEvents(stamped: Stamped[KeyedEvent[Event]]): (FatState, Option[Stamped[KeyedEvent[OrderFatEvent]]]) = {
    if (stamped.eventId <= eventId) throw new IllegalArgumentException(s"stamped.eventId ${EventId.toString(stamped.eventId)} <= eventId ${EventId.toString(eventId)}")
    stamped.value match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
        handleOrderEvent(stamped.copy(value = orderId <-: event))

      case KeyedEvent(_: NoKey, event: RepoEvent) ⇒
        val updatedConverter = copy(
          eventId = stamped.eventId,
          repo = repo.applyEvent(event).orThrow)
        (updatedConverter, None)

      case _ ⇒
        (this, None)
    }
  }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): (FatState, Option[Stamped[KeyedEvent[OrderFatEvent]]]) = {
    val Stamped(eventId, timestamp, KeyedEvent(orderId, event)) = stamped
    val order = event match {
      case event: OrderAdded ⇒ Order.fromOrderAdded(orderId, event)
      case event: OrderCoreEvent ⇒ idToOrder(orderId).update(event)
      case _ ⇒ idToOrder(orderId)
    }
    val updatedFatState = event match {
      case _: OrderAdded      ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder + (order.id → order))
      case _: OrderFinished   ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder - order.id)
      case event: OrderForked ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder ++ (order.newForkedOrders(event) :+ order).map(o ⇒ o.id → o))
      case _: OrderJoined     ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder -- idToOrder(order.id).castState[Order.Join].state.joinOrderIds)
      case _: OrderCoreEvent  ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder + (order.id → order))
      case _ ⇒ this
    }
    val fatEvents = toFatEvent(order, event) map (e ⇒ Stamped(eventId, timestamp, order.id <-: e))
    (updatedFatState, fatEvents)
  }

  private def toFatEvent(order: Order[Order.State], event: OrderEvent): Option[OrderFatEvent] =
    event match {
      case added: OrderAdded ⇒
        Some(OrderAddedFat(added.workflowId /: Position(0), added.scheduledAt, order.variables))

      case _: OrderProcessingStarted ⇒
        val jobPath = repo.idTo[Workflow](order.workflowId).flatMap(_.checkedJob(order.position)).orThrow.jobPath
        val agentUri = order.attachedToAgent.flatMap(a ⇒ repo.idTo[Agent](a)).orThrow.uri
        Some(OrderProcessingStartedFat(order.workflowPosition, agentUri, jobPath, order.variables))

      case OrderStdWritten(stdoutOrStderr, chunk) ⇒
        Some(OrderStdWrittenFat(order.id, stdoutOrStderr)(chunk))

      case event: OrderProcessed ⇒
        Some(OrderProcessedFat(event.outcome, order.variables))

      case OrderFinished ⇒
        Some(OrderFinishedFat(order.workflowPosition))

      case OrderForked(children) ⇒
        Some(OrderForkedFat(
          order.workflowId /: order.position,
          for (ch ← children) yield
            OrderForkedFat.Child(ch.branchId, ch.orderId, ch.variablesDiff.applyTo(order.variables))))

      case _ ⇒
        None
    }
}

object FatState {
  val Initial = FatState(EventId.BeforeFirst, Repo.empty, Map.empty)
}
