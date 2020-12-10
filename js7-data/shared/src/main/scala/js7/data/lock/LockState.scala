package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.data.event.KeyedEvent
import js7.data.lock.Acquired.Available
import js7.data.lock.LockRefusal.LimitReached
import js7.data.order.OrderEvent.{OrderFailedEvent, OrderLockAcquired, OrderLockEvent, OrderLockQueued, OrderLockReleased}
import js7.data.order.OrderId
import scala.collection.immutable.Queue

final case class LockState(lock: Lock, acquired: Acquired = Available, queue: Queue[OrderId] = Queue.empty)
{
  def applyEvent(keyedEvent: KeyedEvent[OrderLockEvent]): Checked[LockState] = {
    assertThat(keyedEvent.event.lockIds contains lock.id)
    keyedEvent match {
      case KeyedEvent(orderId, OrderLockAcquired(lock.id, isExclusive)) =>
        for (lockState <- toLockState(tryAcquire(orderId, isExclusive))) yield
          queue.dequeueOption  match {
            case Some((`orderId`, tail)) => lockState.copy(queue = tail)
            case _ => lockState
          }

      case KeyedEvent(orderId, OrderLockReleased(lock.id)) =>
        release(orderId)

      case KeyedEvent(orderId, OrderLockQueued(lock.id)) =>
        if (acquired == Available)
          Left(Problem(s"$lockId is available an does not accept queuing"))
        else if (acquired.isAcquiredBy(orderId))
          Left(LockRefusal.AlreadyAcquiredByThisOrder.toProblem(lock.id))
        else if (queue contains orderId)
          Left(Problem(s"Order '${orderId.string}' already queues for $lockId"))
        else orderId.allParents find acquired.isAcquiredBy match {
          case Some(parentOrderId) =>
            Left(Problem(s"$lockId has already been acquired by parent $parentOrderId"))
          case None =>
            Right(enqueue(orderId))
        }

      case KeyedEvent(orderId: OrderId, _: OrderFailedEvent) =>
        release(orderId)

      case _ => Left(Problem.pure(s"Invalid event for '$lockId': $keyedEvent"))
    }
  }

  private def enqueue(orderId: OrderId): LockState =
    copy(queue = queue.enqueue(orderId))

  def release(orderId: OrderId): Checked[LockState] =
    toLockState(acquired.release(orderId))

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
      .left.map(refusal => refusal.toProblem(lock.id))
      .map(acquired =>
        copy(acquired = acquired))

  private def lockId = lock.id
}

object LockState
{
  def OrderLockNotAvailableProblem(lockId: LockId) =
    Problem(s"Lock '${lockId.string}' is not available")

  implicit val jsonCodec = deriveCodec[LockState]
}
