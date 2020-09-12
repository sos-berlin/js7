package js7.agent.scheduler.order

import akka.actor.ActorRef
import js7.agent.scheduler.order.OrderRegister._
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.core.common.ActorRegister
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.OrderDetached
import js7.data.order.{Order, OrderId}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import monix.execution.{Cancelable, Scheduler}
import scala.concurrent.Promise

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
    @volatile private[OrderRegister] var timer: Option[Cancelable] = None
    var detachResponses: List[Promise[Unit]] = Nil

    def isDetaching = detachResponses.nonEmpty

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
