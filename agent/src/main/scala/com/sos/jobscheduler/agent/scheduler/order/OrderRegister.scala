package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.monixutils.MonixBase.syntax._
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import monix.execution.{Cancelable, Scheduler}

/**
  * @author Joacim Zschimmer
  */
private[order] final class OrderRegister extends ActorRegister[OrderId, OrderEntry](_.actor) {

  def recover(order: Order[Order.State], workflow: Workflow, actor: ActorRef): OrderEntry = {
    val orderEntry = new OrderEntry(order, workflow, actor)
    insert(order.id -> orderEntry)
    orderEntry
  }

  def handleOrderDetached(keyedEvent: KeyedEvent[OrderDetached]): Unit = {
    this -= keyedEvent.key
  }

  def insert(order: Order[Order.State], workflow: Workflow, actor: ActorRef): Unit = {
    insert(order.id -> new OrderEntry(order, workflow, actor))
  }

  def onActorTerminated(actor: ActorRef): Unit =
    remove(actorToKey(actor))

  override def remove(orderId: OrderId): Option[OrderEntry] =
    for (orderEntry <- super.remove(orderId)) yield {
      orderEntry.timer foreach (_.cancel())
      orderEntry
    }

  def idToOrder: PartialFunction[OrderId, Order[Order.State]] = {
    case orderId if contains(orderId) => apply(orderId).order
  }
}

private[order] object OrderRegister
{
  final class OrderEntry(
    private var _order: Order[Order.State],
    val workflow: Workflow,
    val actor: ActorRef)
  {
    var detaching: Boolean = false
    @volatile private[OrderRegister] var timer: Option[Cancelable] = None

    def order = _order

    def order_=(o: Order[Order.State]) = {
      assertThat(_order.workflowId == o.workflowId)
      _order = o
    }

    def checkedJob: Checked[WorkflowJob] =
      workflow.checkedWorkflowJob(order.position) mapProblem (_ withKey order.id)

    def instruction = workflow.instruction(order.position)

    def at(timestamp: Timestamp)(body: => Unit)(implicit scheduler: Scheduler): Unit = {
      val t = scheduler.scheduleFor(timestamp) {  // TODO What to do when clock is adjusted?
        timer = None
        body
      }
      timer foreach (_.cancel())
      timer = Some(t)
    }
  }
}
