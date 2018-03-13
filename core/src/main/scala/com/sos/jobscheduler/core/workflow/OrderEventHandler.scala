package com.sos.jobscheduler.core.workflow

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.flatMap._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.core.workflow.OrderEventHandler._
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAwaiting, OrderFinished, OrderForked, OrderJoined, OrderOffered, OrderProcessed}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderEventHandler(
  idToWorkflow: WorkflowId ⇒ Checked[Workflow],
  idToOrder: PartialFunction[OrderId, Order[Order.State]])
{
  private val _offeredToAwaitingOrder = mutable.Map[OrderId, Set[OrderId]]()  // FIXME Verschwindet, wenn FileBased erneut eingelesen werden. Event OrderOffered?

  def offeredToAwaitingOrder(orderId: OrderId): Set[OrderId] =
    _offeredToAwaitingOrder.getOrElse(orderId, Set.empty)

  def handleEvent(keyedEvent: KeyedEvent[OrderEvent]): Checked[Seq[FollowUp]] = {
    val KeyedEvent(orderId, event) = keyedEvent
    for {
      previousOrderId ← idToOrder.checked(orderId)
      followUps ← handleEvent(previousOrderId, orderId, event)
    } yield followUps
  }

  private def handleEvent(previousOrder: Order[Order.State], orderId: OrderId, event: OrderEvent): Checked[Seq[FollowUp]] =
    event match {
      case event: OrderProcessed if event.outcome != Outcome.RecoveryGeneratedOutcome ⇒
        for {
          workflow ← idToWorkflow(previousOrder.workflowId)
          job ← workflow.checkedJob(previousOrder.position)
            .mapProblem(_ withPrefix s"Problem with '${previousOrder.id}' in state Processed:")
        } yield
          FollowUp.Processed(job) :: Nil

      case event: OrderForked ⇒
        Valid(previousOrder.newForkedOrders(event) map FollowUp.AddChild.apply)

      case joined: OrderJoined ⇒
        previousOrder.state match {
          case Order.Join(joinOrderIds) ⇒
            Valid(joinOrderIds map FollowUp.Remove.apply)

          case Order.Awaiting(_) ⇒
            _offeredToAwaitingOrder -= previousOrder.castState[Order.Awaiting].state.offeredOrderId
            Valid(Nil)  // Offered order is being kept

          case state ⇒
            Invalid(Problem(s"Event $joined, but Order is in state $state"))
        }

      case event: OrderOffered ⇒
        Valid(FollowUp.AddOffered(previousOrder.newPublishedOrder(event)) :: Nil)

      case OrderAwaiting(offeredOrderId) ⇒
        _offeredToAwaitingOrder(offeredOrderId) = _offeredToAwaitingOrder.getOrElse(offeredOrderId, Set.empty) + orderId
        Valid(Nil)

      case OrderFinished ⇒
        Valid(FollowUp.Remove(orderId) :: Nil)

      case _ ⇒
        Valid(Nil)
    }
}

object OrderEventHandler
{
  sealed trait FollowUp
  object FollowUp {
    final case class Processed(job: Job) extends FollowUp
    final case class AddChild(order: Order[Order.Ready]) extends FollowUp
    final case class AddOffered(order: Order[Order.Offered]) extends FollowUp
    final case class Remove(orderId: OrderId) extends FollowUp
  }
}
