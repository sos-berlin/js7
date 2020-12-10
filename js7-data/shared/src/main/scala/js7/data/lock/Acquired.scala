package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.data.lock.LockRefusal.{AlreadyAcquiredByThisOrder, InvalidCount, IsInUse, UnknownReleasingOrderError}
import js7.data.order.OrderId

sealed trait Acquired {
  def lockCount: Int
  def isAcquiredBy(orderId: OrderId): Boolean
  def acquireFor(orderId: OrderId, count: Option[Int]): Either[LockRefusal, Acquired]
  def release(orderId: OrderId): Either[LockRefusal, Acquired]
}

object Acquired {

  case object Available extends Acquired
  {
    def lockCount = 0

    def isAcquiredBy(orderId: OrderId) = false

    def acquireFor(orderId: OrderId, count: Option[Int]) =
      count match {
        case None => Right(Exclusive(orderId))
        case Some(n) =>
          if (n >= 1) Right(NonExclusive(Map(orderId -> n)))
          else Left(InvalidCount(n))
      }

    def release(orderId: OrderId) =
      Left(UnknownReleasingOrderError)
  }

  final case class Exclusive(orderId: OrderId) extends Acquired
  {
    def lockCount = 1

    def isAcquiredBy(orderId: OrderId) =
      this.orderId == orderId

    def acquireFor(orderId: OrderId, count: Option[Int]) =
      count match {
        case None | Some(1) =>
          if (this.orderId == orderId)
            Left(AlreadyAcquiredByThisOrder)
          else
            Left(IsInUse)
        case Some(n) =>
          Left(InvalidCount(n))
      }

    def release(orderId: OrderId) =
      if (this.orderId != orderId)
        Left(UnknownReleasingOrderError)
      else
        Right(Available)
  }

  final case class NonExclusive(orderToCount: Map[OrderId, Int]) extends Acquired
  {
    assertThat(orderToCount.nonEmpty)
    assertThat(orderToCount.values.forall(_ >= 1))

    def lockCount = orderToCount.values.sum

    def isAcquiredBy(orderId: OrderId) =
      orderToCount contains orderId

    def acquireFor(orderId: OrderId, count: Option[Int]) =
      if (orderToCount contains orderId)
        Left(AlreadyAcquiredByThisOrder)
      else
        count match {
          case None => Left(IsInUse)
          case Some(n) =>
            if (n >= 1) Right(NonExclusive(orderToCount + (orderId -> n)))
            else Left(InvalidCount(n))
        }

    def release(orderId: OrderId) =
      if (!orderToCount.contains(orderId))
        Left(UnknownReleasingOrderError)
      else
        Right(
          if (orderToCount.size == 1)
            Available
          else
            copy(orderToCount = orderToCount - orderId))
  }

  sealed trait ReleaseError

  implicit val jsonCodec = TypedJsonCodec[Acquired](
    Subtype(deriveCodec[Exclusive]),
    Subtype(deriveCodec[NonExclusive]),
    Subtype(Available))
}

