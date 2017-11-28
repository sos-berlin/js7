package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.Workflow.{JobNode, Node}
import com.sos.jobscheduler.shared.common.ActorRegister
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderRegister(timerService: TimerService) extends ActorRegister[OrderId, OrderEntry](_.actor) {

  def recover(order: Order[Order.State], workflow: Workflow, actor: ActorRef): OrderEntry = {
    val orderEntry = new OrderEntry(order, workflow, actor)
    insert(order.id → orderEntry)
    orderEntry
  }

  def handleOrderDetached(keyedEvent: KeyedEvent[OrderDetached.type]): Unit = {
    this -= keyedEvent.key
  }

  def insert(order: Order[Order.State], workflow: Workflow, actor: ActorRef): Unit = {
    insert(order.id → new OrderEntry(order, workflow, actor))
  }

  def onActorTerminated(actor: ActorRef)(implicit timerService: TimerService): Unit = {
    for (orderEntry ← remove(actorToKey(actor))) {
      orderEntry.timer foreach timerService.cancel
    }
  }
}

private[order] object OrderRegister {

  final class OrderEntry(
    private var _order: Order[Order.State],
    private val workflow: Workflow,
    val actor: ActorRef)
  {
    assert(workflow.path == _order.workflowPath)

    var detaching: Boolean = false
    private[OrderRegister] var timer: Option[Timer[Unit]] = None

    def order = _order

    def order_=(o: Order[Order.State]) = {
      assert(workflow.path == o.workflowPath)
      _order = o
    }

    def jobNodeOption: Option[JobNode] =
      workflow.jobNodeOption(order.nodeId)

    def jobNode: JobNode =
      workflow.jobNode(order.nodeId)

    def nodeOption: Option[Node] =
      workflow.idToNode.get(order.nodeId)

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
