package com.sos.jobscheduler.master.web.master.api.fatevent

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.Agent
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import com.sos.jobscheduler.data.order.OrderFatEvent.{OrderAddedFat, OrderFinishedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdWrittenFat}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderFatEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Position, Workflow}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
private[fatevent] final case class FatState(eventId: EventId, repo: Repo, idToOrder: Map[OrderId, Order[Order.State]])
{
  def toFatOrderEvents(stamped: Stamped[KeyedEvent[Event]]): (FatState, Seq[Stamped[KeyedEvent[OrderFatEvent]]]) =
    synchronized {
      if (stamped.eventId <= eventId) throw new IllegalArgumentException(s"stamped.eventId ${EventId.toString(stamped.eventId)} <= eventId ${EventId.toString(eventId)}")
      stamped.value match {
        case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
          handleOrderEvent(stamped.copy(value = orderId <-: event))

        case KeyedEvent(_: NoKey, event: RepoEvent) ⇒
          val updatedConverter = copy(
            eventId = stamped.eventId,
            repo = repo.applyEvent(event).orThrow)
          (updatedConverter, Nil)

        case _ ⇒
          (this, Nil)
      }
    }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): (FatState, Seq[Stamped[KeyedEvent[OrderFatEvent]]]) = {
    val Stamped(eventId, timestamp, KeyedEvent(orderId, event)) = stamped
    val order = event match {
      case event: OrderAdded ⇒ Order.fromOrderAdded(orderId, event)
      case event: OrderCoreEvent ⇒ idToOrder(orderId).update(event)
      case _ ⇒ idToOrder(orderId)
    }
    val fatEvents = toFatEvent(order, event) map (e ⇒ Stamped(eventId, timestamp, e))
    val updatedConverter = event match {
      case _: OrderAdded      ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder + (order.id → order))
      case _: OrderFinished   ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder - order.id)
      case event: OrderForked ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder ++ (order.newForkedOrders(event) :+ order).map(o ⇒ o.id → o))
      case _: OrderJoined     ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder -- idToOrder(order.id).castState[Order.Join].state.joinOrderIds)
      case _: OrderCoreEvent  ⇒ copy(eventId = stamped.eventId, idToOrder = idToOrder + (order.id → order))
      case _ ⇒ this
    }
    (updatedConverter, fatEvents)
  }

  private def toFatEvent(order: Order[Order.State], event: OrderEvent): Seq[KeyedEvent[OrderFatEvent]] =
    event match {
      case added: OrderAdded ⇒
        (order.id <-: OrderAddedFat(None, OrderAddedFat.Cause.UNKNOWN, added.workflowId /: Position(0), added.scheduledAt, order.variables)) :: Nil

      case _: OrderProcessingStarted ⇒
        val jobPath = repo.idTo[Workflow](order.workflowId).flatMap(_.checkedJob(order.position)).orThrow.jobPath
        val agentUri = order.attachedToAgent.flatMap(a ⇒ repo.idTo[Agent](a)).orThrow.uri
        (order.id <-: OrderProcessingStartedFat(order.workflowPosition, agentUri, jobPath, order.variables)) :: Nil

      case OrderStdWritten(stdoutOrStderr, chunk) ⇒
        (order.id <-: OrderStdWrittenFat(order.id, stdoutOrStderr)(chunk)) :: Nil

      case event: OrderProcessed ⇒
        (order.id <-: OrderProcessedFat(event.outcome, order.variables)) :: Nil

      case OrderFinished ⇒
        val event = order.id <-: OrderFinishedFat(order.workflowPosition)
        event :: Nil

      case event: OrderForked ⇒
        for (childOrder ← order.newForkedOrders(event)) yield
          childOrder.id <-: OrderAddedFat(Some(order.id), OrderAddedFat.Cause.Forked, childOrder.workflowPosition, None, childOrder.variables)

      case _ ⇒
        Nil
    }
}

object FatState {
  val Initial = FatState(EventId.BeforeFirst, Repo.empty, Map.empty)
}
