package js7.data.state

import js7.base.problem.{Checked, Problem}
import js7.data.job.JobKey
import js7.data.order.OrderEvent.{OrderCancelled, OrderDeleted, OrderDetachable, OrderForked, OrderJoined, OrderProcessed}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.workflow.{Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
final class OrderEventHandler(idToWorkflow: WorkflowId => Checked[Workflow]):
  def handleEvent(previousOrder: Order[Order.State], event: OrderEvent): Checked[Seq[FollowUp]] =
    event match
      case _: OrderProcessed =>
        for
          workflow <- idToWorkflow(previousOrder.workflowId)
          jobKey <- workflow.positionToJobKey(previousOrder.position)
        yield
          FollowUp.LeaveJob(jobKey) :: Nil

      case OrderCancelled | OrderDetachable =>
        // Order may be enqueued in Job's queue, still waiting for start allowance
        for workflow <- idToWorkflow(previousOrder.workflowId) yield
          workflow
            .positionToJobKey(previousOrder.position)
            .toOption
            .map(FollowUp.LeaveJob(_))
            .toList

      case event: OrderForked =>
        Right(previousOrder
          .newForkedOrders(event)
          .map(FollowUp.AddChild(_)))

      case joined: OrderJoined =>
        previousOrder.state match
          case o: Order.Forked =>
            Right(o.children
              .map(o => FollowUp.Delete(o.orderId)))

          case state =>
            Left(Problem(s"Event $joined, but Order is in state $state"))

      case _: OrderDeleted =>
        Right(FollowUp.Delete(previousOrder.id) :: Nil)

      case _ =>
        Right(Nil)


object OrderEventHandler:
  sealed trait FollowUp
  object FollowUp:
    final case class LeaveJob(job: JobKey) extends FollowUp
    final case class AddChild(order: Order[Order.Ready]) extends FollowUp
    final case class Delete(orderId: OrderId) extends FollowUp
