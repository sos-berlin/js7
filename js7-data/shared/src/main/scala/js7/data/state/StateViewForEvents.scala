package js7.data.state

import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderDetachable, OrderMoved}
import js7.data.workflow.WorkflowPathControlPath

object StateViewForEvents:

  extension (state: StateView)

    def atController(events: => List[OrderActorEvent]): List[OrderActorEvent] =
      if state.isAgent then
        OrderDetachable :: Nil
      else
        events

    def ifSkippedThenMove(order: Order[Order.State]): Option[OrderMoved] =
      !order.isFailed ?&
        skippedReason(order).map: reason =>
          OrderMoved(order.position.increment, Some(reason))

    private def skippedReason(order: Order[Order.State]): Option[OrderMoved.Reason] =
      state.isSkippedDueToWorkflowPathControl(order) ?
        OrderMoved.SkippedDueToWorkflowPathControl

    private def isSkippedDueToWorkflowPathControl(order: Order[Order.State]): Boolean =
      !order.isState[Order.BetweenCycles] &&
        state
          .pathToWorkflowPathControl.get(WorkflowPathControlPath(order.workflowPath))
          .exists: control =>
            state.workflowPositionToLabel(order.workflowPosition)
              .toOption.flatten
              .exists(control.skip.contains)
