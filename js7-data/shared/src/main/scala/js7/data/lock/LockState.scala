package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.data.event.KeyedEvent
import js7.data.lock.Acquired.Available
import js7.data.lock.LockRefusal.LimitReached
import js7.data.order.OrderEvent.{OrderLockAcquired, OrderLockEvent, OrderLockQueued, OrderLockReleased}
import js7.data.order.OrderId
import scala.collection.immutable.Queue

final case class LockState(lock: Lock, acquired: Acquired = Available, queue: Queue[OrderId] = Queue.empty)
{
  def applyEvent(keyedEvent: KeyedEvent[OrderLockEvent]): Checked[LockState] =
    keyedEvent match {
      case KeyedEvent(orderId, OrderLockAcquired(lock.name, isExclusive)) =>
        for (lockState <- toLockState(tryAcquire(orderId, isExclusive))) yield
          queue.dequeueOption  match {
            case Some((`orderId`, tail)) => lockState.copy(queue = tail)
            case _ => lockState
          }

      case KeyedEvent(orderId, OrderLockReleased(lock.name)) =>
        toLockState(acquired.release(orderId))

      case KeyedEvent(orderId, OrderLockQueued(lock.name)) =>
        if (acquired == Available)
          Left(Problem(s"${lock.name} is available an does not accept queuing"))
        else if (acquired.isAcquiredBy(orderId))
          Left(LockRefusal.AlreadyAcquiredByThisOrder.toProblem(lock.name))
        else if (queue contains orderId)
          Left(Problem(s"Order '${orderId.string}' already queues for ${lock.name}"))
        else
          Right(enqueue(orderId))

      case _ => Left(Problem.pure(s"Invalid event for '${lock.name}': $keyedEvent"))
    }

  private def enqueue(orderId: OrderId): LockState =
    copy(queue = queue.enqueue(orderId))

  def firstQueuedOrderId: Option[OrderId] =
    queue.headOption

  def checkAcquire(orderId: OrderId, exclusive: Boolean): Either[LockRefusal, Unit] =
    tryAcquire(orderId, exclusive)
      .map(_ => ())

  private def tryAcquire(orderId: OrderId, exclusive: Boolean): Either[LockRefusal, Acquired] =
    for {
      a <- acquired.acquireFor(orderId, exclusive)
      _ <- checkLimit(exclusive)
    } yield a

  private def checkLimit(exclusive: Boolean): Either[LockRefusal, Unit] =
    if (acquired.lockCount < (if (exclusive) 1 else lock.limit))
      Right(())
    else
      Left(LimitReached(lock.limit))

  private def toLockState(result: Either[LockRefusal, Acquired]): Checked[LockState] =
    result
      .left.map(refusal => refusal.toProblem(lock.name))
      .map(acquired =>
        copy(acquired = acquired))
}

object LockState
{
  def OrderLockNotAvailableProblem(lockName: LockName) =
    Problem(s"Lock '${lockName.string}' is not available")

  implicit val jsonCodec = deriveCodec[LockState]
}
