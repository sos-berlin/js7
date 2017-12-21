package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowGraph
import com.sos.jobscheduler.shared.common.ActorRegister
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderRegister(timerService: TimerService) extends ActorRegister[OrderId, OrderEntry](_.actor) {

  def recover(order: Order[Order.State], workflowGraph: WorkflowGraph, actor: ActorRef): OrderEntry = {
    val orderEntry = new OrderEntry(order, workflowGraph, actor)
    insert(order.id → orderEntry)
    orderEntry
  }

  def handleOrderDetached(keyedEvent: KeyedEvent[OrderDetached.type]): Unit = {
    this -= keyedEvent.key
  }

  def insert(order: Order[Order.State], workflowGraph: WorkflowGraph, actor: ActorRef): Unit = {
    insert(order.id → new OrderEntry(order, workflowGraph, actor))
  }

  def onActorTerminated(actor: ActorRef)(implicit timerService: TimerService): Unit = {
    for (orderEntry ← remove(actorToKey(actor))) {
      orderEntry.timer foreach timerService.cancel
    }
  }

  def idToOrder: PartialFunction[OrderId, Order[Order.State]] = {
    case orderId if contains(orderId) ⇒ apply(orderId).order
  }
}

private[order] object OrderRegister {

  final class OrderEntry(
    private var _order: Order[Order.State],
    val workflowGraph: WorkflowGraph,
    val actor: ActorRef)
  {
    var detaching: Boolean = false
    private[OrderRegister] var timer: Option[Timer[Unit]] = None

    def order = _order

    def order_=(o: Order[Order.State]) = {
      assert(_order.workflowPath == o.workflowPath)
      _order = o
    }

    def jobNodeOption: Option[WorkflowGraph.JobNode] =
      workflowGraph.jobNodeOption(order.nodeId)

    def jobNode: WorkflowGraph.JobNode =
      workflowGraph.jobNode(order.nodeId)

    def nodeOption: Option[WorkflowGraph.Node] =
      workflowGraph.idToNode.get(order.nodeId)

    def at(timestamp: Timestamp)(body: ⇒ Unit)(implicit timerService: TimerService, ec: ExecutionContext): Unit = {
      val t = timerService.at(timestamp.toInstant, name = order.id.string)
      t onElapsed {
        timer = None
        body
      }
      timer foreach timerService.cancel
      timer = Some(t)
    }
  }
}
