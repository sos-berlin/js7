package com.sos.jobscheduler.master.order.fat

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.Agent
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import com.sos.jobscheduler.data.order.OrderFatEvent.{OrderAddedFat, OrderFinishedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdWrittenFat}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderFatEvent, OrderId}
import com.sos.jobscheduler.data.workflow.{Position, Workflow}
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class StatefulEventToFatOrderEventConverter
{
  private var repo = Repo.empty
  private val idToOrder = mutable.LinkedHashMap[OrderId, Order[Order.State]]()

  def toFatOrderEvents(stamped: Stamped[KeyedEvent[Event]]): Seq[Stamped[KeyedEvent[OrderFatEvent]]] =
    stamped.value match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
        handleOrderEvent(stamped.copy(value = orderId <-: event))

      case KeyedEvent(_: NoKey, event: RepoEvent) ⇒
        repo = repo.applyEvent(event).orThrow
        Nil

      case _ ⇒
        Nil
    }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): Seq[Stamped[KeyedEvent[OrderFatEvent]]] = {
    val Stamped(eventId, timestamp, KeyedEvent(orderId, event)) = stamped
    val order = event match {
      case event: OrderAdded ⇒ Order.fromOrderAdded(orderId, event)
      case event: OrderCoreEvent ⇒ idToOrder(orderId).update(event)
      case _ ⇒ idToOrder(orderId)
    }
    val result = toFatEvent(order, event) map (e ⇒ Stamped(eventId, timestamp, e))
    event match {
      case _: OrderAdded      ⇒ idToOrder.insert(order.id → order)
      case _: OrderFinished   ⇒ idToOrder -= order.id
      case event: OrderForked ⇒ idToOrder ++= (order.newForkedOrders(event) :+ order).map(o ⇒ o.id → o)
      case _: OrderJoined     ⇒ idToOrder --= idToOrder(order.id).castState[Order.Join].state.joinOrderIds
      case _: OrderCoreEvent  ⇒ idToOrder += order.id → order
      case _ ⇒
    }
    result
  }

  private def toFatEvent(order: Order[Order.State], event: OrderEvent): Seq[KeyedEvent[OrderFatEvent]] =
    event match {
      case added: OrderAdded ⇒
        (order.id <-: OrderAddedFat(None, OrderAddedFat.Cause.UNKNOWN, added.workflowId /: Position(0), added.scheduledAt)) :: Nil

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
          childOrder.id <-: OrderAddedFat(Some(order.id), OrderAddedFat.Cause.Forked, childOrder.workflowPosition, None)

      case _ ⇒
        Nil
    }
}
