package js7.data.lock

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.data.lock.LockRefusal.{AlreadyAcquiredByThisOrder, InvalidCount, IsInUse, UnknownReleasingOrderError}
import js7.data.order.OrderId

sealed trait Acquired:
  def lockCount: Int
  def orderIds: Iterable[OrderId]
  def isAcquiredBy(orderId: OrderId): Boolean
  def acquireFor(orderId: OrderId, count: Option[Int]): Either[LockRefusal, Acquired]
  def release(orderId: OrderId): Either[LockRefusal, Acquired]


object Acquired:

  case object Available extends Acquired:
    def lockCount = 0

    def orderIds: Seq[OrderId] =
      Nil

    def isAcquiredBy(orderId: OrderId) = false

    def acquireFor(orderId: OrderId, count: Option[Int]): Either[LockRefusal, Acquired] =
      count match
        case None => Right(Exclusive(orderId))
        case Some(n) =>
          if n >= 1 then Right(NonExclusive(Map(orderId -> n)))
          else Left(InvalidCount(n))

    def release(orderId: OrderId): Left[LockRefusal, Nothing] =
      Left(UnknownReleasingOrderError)

  final case class Exclusive(orderId: OrderId) extends Acquired:
    def lockCount = 1

    def orderIds: Seq[OrderId] =
      orderId :: Nil

    def isAcquiredBy(orderId: OrderId): Boolean =
      this.orderId == orderId

    def acquireFor(orderId: OrderId, count: Option[Int]): Left[LockRefusal, Nothing] =
      if this.orderId == orderId then
        Left(AlreadyAcquiredByThisOrder)
      else
        Left(IsInUse)

    def release(orderId: OrderId): Either[LockRefusal, Available.type] =
      if this.orderId != orderId then
        Left(UnknownReleasingOrderError)
      else
        Right(Available)

  final case class NonExclusive(orderToCount: Map[OrderId, Int]) extends Acquired:
    assertThat(orderToCount.nonEmpty)
    assertThat(orderToCount.values.forall(_ >= 1))

    def lockCount: Int =
      orderToCount.values.sum

    def orderIds: Iterable[OrderId] =
      orderToCount.keys

    def isAcquiredBy(orderId: OrderId): Boolean =
      orderToCount contains orderId

    def acquireFor(orderId: OrderId, count: Option[Int]): Either[LockRefusal, NonExclusive] =
      if orderToCount contains orderId then
        Left(AlreadyAcquiredByThisOrder)
      else
        count match
          case None => Left(IsInUse)
          case Some(n) =>
            if n >= 1 then Right(NonExclusive(orderToCount + (orderId -> n)))
            else Left(InvalidCount(n))

    def release(orderId: OrderId): Either[LockRefusal, Acquired] =
      if !orderToCount.contains(orderId) then
        Left(UnknownReleasingOrderError)
      else
        Right(
          if orderToCount.size == 1 then
            Available
          else
            copy(orderToCount = orderToCount - orderId))

  implicit val jsonCodec: TypedJsonCodec[Acquired] = TypedJsonCodec(
    Subtype(deriveCodec[Exclusive]),
    Subtype(deriveCodec[NonExclusive]),
    Subtype(Available))
