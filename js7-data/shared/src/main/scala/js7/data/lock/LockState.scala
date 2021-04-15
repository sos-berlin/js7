package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Big
import js7.data.event.KeyedEvent
import js7.data.item.SimpleItemState
import js7.data.lock.Acquired.Available
import js7.data.lock.LockRefusal.{InvalidCount, IsInUse, LimitReached}
import js7.data.order.OrderEvent.{OrderFailedEvent, OrderLockAcquired, OrderLockEvent, OrderLockQueued, OrderLockReleased}
import js7.data.order.OrderId
import scala.collection.immutable.Queue

final case class LockState(
  lock: Lock,
  acquired: Acquired = Available,
  queue: Queue[OrderId] = Queue.empty)
extends SimpleItemState with Big/*acquired and queue get big with many orders*/
{
  import lock.limit

  def item = lock

  def agentIdToAttachedState = Map.empty

  def applyEvent(keyedEvent: KeyedEvent[OrderLockEvent]): Checked[LockState] = {
    assertThat(keyedEvent.event.lockIds contains lock.id)
    keyedEvent match {
      case KeyedEvent(orderId, OrderLockAcquired(lock.id, count)) =>
        for (lockState <- toLockState(tryAcquire(orderId, count))) yield
          if (queue contains orderId)
            lockState.copy(queue = queue.filterNot(_ == orderId)) /*TODO Slow with long order queue*/
          else
            lockState

      case KeyedEvent(orderId, OrderLockReleased(lock.id)) =>
        release(orderId)

      case KeyedEvent(orderId, OrderLockQueued(lock.id, count)) =>
        if (!count.forall(_ <= limit))
          Left(Problem(s"Cannot fulfill lock count=${count getOrElse ""} with $lockId limit=$limit"))
        else if (acquired == Available)
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

  def checkAcquire(orderId: OrderId, count: Option[Int] = None): Either[LockRefusal, Unit] =
    tryAcquire(orderId, count)
      .map(_ => ())

  private def tryAcquire(orderId: OrderId, count: Option[Int]): Either[LockRefusal, Acquired] =
    for {
      a <- acquired.acquireFor(orderId, count)
      _ <- checkLimit(count)
    } yield a

  private def checkLimit(count: Option[Int] = None): Either[LockRefusal, Unit] =
    count match {
      case None =>
        if (acquired == Available)
          Right(())
        else
          Left(IsInUse)

      case Some(n) =>
        if (n < 1)
          Left(InvalidCount(n))
        else {
          val ok =
            try math.addExact(acquired.lockCount, n) <= limit
            catch { case _: ArithmeticException => false }
          if (ok)
            Right(())
          else
            Left(LimitReached(limit = limit, count = acquired.lockCount, n))
        }
    }

  private def toLockState(result: Either[LockRefusal, Acquired]): Checked[LockState] =
    result
      .left.map(refusal => refusal.toProblem(lock.id))
      .map(acquired =>
        copy(acquired = acquired))

  private def lockId = lock.id

  //TODO Break snapshot into smaller parts: private def toSnapshot: Observable[Any] = ...
}

object LockState
{
  def OrderLockNotAvailableProblem(lockId: LockId) =
    Problem(s"Lock '${lockId.string}' is not available")

  implicit val jsonCodec = deriveCodec[LockState]
}
