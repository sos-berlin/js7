package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.instructions.Job
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

  def handleOrderDetached(keyedEvent: KeyedEvent[OrderDetached]): Unit = {
    this -= keyedEvent.key
  }

  def insert(order: Order[Order.State], workflow: Workflow, actor: ActorRef): Unit = {
    insert(order.id → new OrderEntry(order, workflow, actor))
  }

  def onActorTerminated(actor: ActorRef): Unit =
    remove(actorToKey(actor))

  override def remove(orderId: OrderId): Option[OrderEntry] =
    for (orderEntry ← super.remove(orderId)) yield {
      orderEntry.timer foreach timerService.cancel
      orderEntry
    }

  def idToOrder: PartialFunction[OrderId, Order[Order.State]] = {
    case orderId if contains(orderId) ⇒ apply(orderId).order
  }
}

private[order] object OrderRegister {

  final class OrderEntry(
    private var _order: Order[Order.State],
    val workflow: Workflow,
    val actor: ActorRef)
  {
    var detaching: Boolean = false
    private[OrderRegister] var timer: Option[Timer[Unit]] = None

    def order = _order

    def order_=(o: Order[Order.State]) = {
      assert(_order.workflowPath == o.workflowPath)
      _order = o
    }

    def checkedJob: Checked[Job] =
      workflow.checkedJob(order.position) withProblemKey order.id

    def jobOption: Option[Job] =
      workflow.jobOption(order.position)

    def instruction = workflow.instruction(order.position)

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
