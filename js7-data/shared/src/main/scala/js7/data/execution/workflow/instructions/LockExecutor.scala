package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.StateView
import js7.data.lock.LockRefusal
import js7.data.order.Order
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderActorEvent, OrderDetachable, OrderLockAcquired, OrderLockQueued, OrderLockReleased, OrderStarted}
import js7.data.workflow.instructions.LockInstruction

object LockExecutor extends EventInstructionExecutor
{
  type Instr = LockInstruction

  def toEvents(instruction: LockInstruction, order: Order[Order.State], state: StateView) = {
    import instruction.{count, lockPath}

    if (order.isAttached)
      Right((order.id <-: OrderDetachable) :: Nil)
    else if (order.isState[Fresh])
      Right((order.id <-: OrderStarted) :: Nil)
    else if (order.isState[Order.Ready] || order.isState[Order.WaitingForLock])
      for {
        lockState <- state.pathToLockState(lockPath)
        event <- lockState.checkAcquire(order.id, count) match {
          case Right(()) =>
            Right(Some(OrderLockAcquired(lockPath, count)))

          case Left(refusal @ (LockRefusal.IsInUse | _: LockRefusal.LimitReached)) =>
            scribe.debug(s"Order '${order.id.string}': ${refusal.toProblem(lockPath).toString}, $lockState")
            Right(!order.isState[Order.WaitingForLock] ? OrderLockQueued(lockPath, count))

          case Left(refusal) =>
            Left(refusal.toProblem(lockPath))
        }
        maybeKeyedEvent = event.map(order.id <-: _)
        _ <- maybeKeyedEvent match {
          case None => Right(None)
          case Some(keyedEvent) => for {
            _ <- lockState.applyEvent(keyedEvent)
            _ <- order.applyEvent(keyedEvent.event)
          } yield ()
        }
      } yield maybeKeyedEvent.toList
    else
      Right(Nil)
  }

  private[workflow] def onReturnFromSubworkflow(order: Order[Order.State], instruction: LockInstruction)
  : Option[KeyedEvent[OrderActorEvent]] =
    Some(order.id <-: (
      if (order.isAttached)
        OrderDetachable
      else
        OrderLockReleased(instruction.lockPath)))
}
