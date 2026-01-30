package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.{fail, moveOrder}
import js7.data.execution.workflow.instructions.RetryExecutor.*
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderRetrying}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{Order, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.EngineEventColl.extensions.order
import js7.data.state.EngineState_
import js7.data.workflow.instructions.{Retry, TryInstruction}
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*

private object RetryExecutor extends EventInstructionExecutor_[Retry]:

  def toEventCalc[S <: EngineState_[S]](instr: Retry, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrderAndWorkflow(orderId): (coll, order, workflow) =>
      val orderId = order.id
      if !order.isState[Order.Ready] then
        coll.nix
      else
        order.workflowPosition.position.nextRetryPosition.flatMap: nextTryPos =>
          nextTryPos.branchPath.parent.flatMap: parent =>
            Some(workflow.instruction(parent)).collect:
              case o: TryInstruction => o
            .flatMap: _try =>
              nextTryPos.branchPath.lastOption.map(_.branchId).collect:
                case TryBranchId(index) => (_try.maxTries, _try.retryDelay(index))
          .toChecked:
            Problem(s"Retry: Order is not in a 'catch' block: $nextTryPos")
          .flatMap:
            case (Some(maxRetries), _) if order.position.tryCount >= maxRetries =>
              coll:
                fail[S](orderId)
            case (_, delay) =>
              if delay.isPositive then
                val nextTimestamp = coll.context.now + delay match
                  case at if delay >= 10.s => at.roundToNextSecond
                  case at => at
                coll:
                  orderId <-: OrderRetrying(Some(nextTimestamp))
              else
                for
                  coll <- coll:
                    order.id <-: OrderRetrying()
                  coll <- coll:
                    moveOrder(order.id, nextTryPos)
                yield coll

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator,
    now: Timestamp)
  : Checked[Set[OrderObstacle]] =
    order.state match
      case Order.DelayingRetry(until) =>
        Right(Set(WaitingForOtherTime(until)))

      case Order.DelayedAfterError(until) =>
        Right(Set(WaitingForOtherTime(until)))

      case _ =>
        super.toObstacles(order, calculator, now)
