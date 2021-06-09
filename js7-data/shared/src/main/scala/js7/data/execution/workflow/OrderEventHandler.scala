package js7.data.execution.workflow

import js7.base.problem.{Checked, Problem}
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.job.JobKey
import js7.data.order.OrderEvent.{OrderAwaiting, OrderDeleted, OrderForked, OrderJoined, OrderOffered, OrderProcessed}
import js7.data.order.{Order, OrderEvent, OrderId, Outcome}
import js7.data.workflow.{Workflow, WorkflowId}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderEventHandler(
  idToWorkflow: WorkflowId => Checked[Workflow],
  idToOrder: OrderId => Checked[Order[Order.State]])
{
  private val _offeredToAwaitingOrder = mutable.Map.empty[OrderId, Set[OrderId]]  // FIXME Verschwindet, wenn VersionedItem erneut eingelesen werden. Event OrderOffered?

  def offeredToAwaitingOrder(orderId: OrderId): Set[OrderId] =
    _offeredToAwaitingOrder.getOrElse(orderId, Set.empty)

  def handleEvent(keyedEvent: KeyedEvent[OrderEvent]): Checked[Seq[FollowUp]] = {
    val KeyedEvent(orderId, event) = keyedEvent
    for {
      previousOrder <- idToOrder(orderId)
      followUps <- handleEvent(previousOrder, orderId, event)
    } yield followUps
  }

  private def handleEvent(previousOrder: Order[Order.State], orderId: OrderId, event: OrderEvent): Checked[Seq[FollowUp]] =
    event match {
      case event: OrderProcessed if event.outcome != Outcome.RecoveryGeneratedOutcome =>
        for {
          workflow <- idToWorkflow(previousOrder.workflowId)
          jobKey <- workflow.positionToJobKey(previousOrder.position)
        } yield
          FollowUp.Processed(jobKey) :: Nil

      case event: OrderForked =>
        Right(previousOrder.newForkedOrders(event) map FollowUp.AddChild.apply)

      case joined: OrderJoined =>
        previousOrder.state match {
          case o: Order.Forked =>
            Right(o.childOrderIds map FollowUp.Delete.apply)

          case Order.Awaiting(_) =>
            val offeredOrderId = previousOrder.castState[Order.Awaiting].state.offeredOrderId
            _offeredToAwaitingOrder -= offeredOrderId
            // Offered order is being kept ???
            //Right(FollowUp.Delete(offeredOrderId) :: Nil)
            Right(Nil)

          case state =>
            Left(Problem(s"Event $joined, but Order is in state $state"))
        }

      case event: OrderOffered =>
        Right(FollowUp.AddOffered(previousOrder.newOfferedOrder(event)) :: Nil)

      case OrderAwaiting(offeredOrderId) =>
        _offeredToAwaitingOrder(offeredOrderId) = _offeredToAwaitingOrder.getOrElse(offeredOrderId, Set.empty) + orderId
        Right(Nil)

      case _: OrderDeleted =>
        Right(FollowUp.Delete(orderId) :: Nil)

      case _ =>
        Right(Nil)
    }
}

object OrderEventHandler
{
  sealed trait FollowUp
  object FollowUp {
    final case class Processed(job: JobKey) extends FollowUp
    final case class AddChild(order: Order[Order.Ready]) extends FollowUp
    final case class AddOffered(order: Order[Order.Offering]) extends FollowUp
    final case class Delete(orderId: OrderId) extends FollowUp
  }
}
