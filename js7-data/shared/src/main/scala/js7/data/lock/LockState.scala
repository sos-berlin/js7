package js7.data.lock

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Big
import js7.data.item.UnsignedSimpleItemState
import js7.data.lock.Acquired.Available
import js7.data.lock.LockRefusal.{InvalidCount, IsInUse, LimitReached}
import js7.data.order.OrderId
import scala.collection.immutable.Queue

final case class LockState(
  lock: Lock,
  acquired: Acquired = Available,
  queue: Queue[OrderId] = Queue.empty)
extends UnsignedSimpleItemState, Big/*acquired and queue get big, many orders*/:

  import lock.limit

  protected type Self = LockState
  val companion: LockState.type = LockState

  val item: Lock = lock
  def path: LockPath = item.path

  def updateItem(lock: Lock): Checked[LockState] =
    Right(copy(lock = lock))

  def enqueue(orderId: OrderId, count: Option[Int]): Checked[LockState] =
    if !count.forall(_ <= limit) then
      Left(Problem(s"Cannot fulfill lock count=${count getOrElse ""} with $lockPath limit=$limit"))
    else if acquired.isAcquiredBy(orderId) then
      Left(LockRefusal.AlreadyAcquiredByThisOrder.toProblem(lock.path))
    else if queue contains orderId then
      Left(Problem(s"$orderId already queues for $lockPath"))
    else orderId.allParents.find(acquired.isAcquiredBy) match
      case Some(parentOrderId) =>
        Left(Problem(s"$lockPath has already been acquired by parent $parentOrderId"))
      case None =>
        Right(copy(
          queue = queue.enqueue(orderId)))

  def isAvailable(orderId: OrderId, count: Option[Int] = None): Checked[Boolean] =
    checkAcquire(orderId, count) match
      case Left(_: LockRefusal.NotAvailable) =>
        Right(false)

      case Left(refusal) =>
        Left(refusal.toProblem(lockPath))

      case Right(()) =>
        Right(true)

  def checkAcquire(orderId: OrderId, count: Option[Int] = None): Either[LockRefusal, Unit] =
    tryAcquire(orderId, count)
      .map(_ => ())

  private def tryAcquire(orderId: OrderId, count: Option[Int]): Either[LockRefusal, Acquired] =
    for
      a <- acquired.acquireFor(orderId, count)
      _ <- checkLimit(count)
    yield a

  def acquire(orderId: OrderId, count: Option[Int]): Checked[LockState] =
    for lockState <- toLockState(tryAcquire(orderId, count)) yield
      if queue contains orderId then
        lockState.copy(
          queue = queue.filterNot(_ == orderId)) /*TODO Slow with long order queue*/
      else
        lockState

  def dequeue(orderId: OrderId): LockState =
    copy(queue = queue.filterNot(_ == orderId))

  def release(orderId: OrderId): Checked[LockState] =
    toLockState(acquired.release(orderId))

  def firstQueuedOrderId: Option[OrderId] =
    queue.headOption

  private def checkLimit(count: Option[Int]): Either[LockRefusal, Unit] =
    count match
      case None =>
        if acquired == Available then
          Right(())
        else
          Left(IsInUse)

      case Some(n) =>
        if n < 1 then
          Left(InvalidCount(n))
        else
          val ok =
            try math.addExact(acquired.lockCount, n) <= limit
            catch { case _: ArithmeticException => false }
          if ok then
            Right(())
          else
            Left(LimitReached(limit = limit, count = n, alreadyRequiredCount = acquired.lockCount))

  private def toLockState(result: Either[LockRefusal, Acquired]): Checked[LockState] =
    result
      .left.map(refusal => refusal.toProblem(lock.path))
      .map(acquired =>
        copy(acquired = acquired))

  private def lockPath = lock.path

  //TODO Break snapshot into smaller parts: private def toSnapshot: Observable[Any] = ...


object LockState extends UnsignedSimpleItemState.Companion[LockState]:
  type Key = LockPath
  type Item = Lock
  override type ItemState = LockState

  implicit val jsonCodec: Codec.AsObject[LockState] = deriveCodec[LockState]
