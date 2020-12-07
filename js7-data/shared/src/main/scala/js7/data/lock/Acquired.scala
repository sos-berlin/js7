package js7.data.lock

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Assertions.assertThat
import js7.data.lock.LockRefusal.{AlreadyAcquiredByThisOrder, IsInUse, UnknownReleasingOrderError}
import js7.data.order.OrderId

sealed trait Acquired {
  def lockCount: Int
  def isAcquiredBy(orderId: OrderId): Boolean
  def acquireFor(orderId: OrderId, isExclusive: Boolean): Either[LockRefusal, Acquired]
  def release(orderId: OrderId): Either[LockRefusal, Acquired]
}

object Acquired {

  case object Available extends Acquired
  {
    def lockCount = 0

    def isAcquiredBy(orderId: OrderId) = false

    def acquireFor(orderId: OrderId, isExclusive: Boolean) =
      Right(
        if (isExclusive)
          Exclusive(orderId)
        else
          NonExclusiv(Set(orderId)))

    def release(orderId: OrderId) =
      Left(UnknownReleasingOrderError)
  }

  final case class Exclusive(orderId: OrderId) extends Acquired
  {
    def lockCount = 1

    def isAcquiredBy(orderId: OrderId) =
      this.orderId == orderId

    def acquireFor(orderId: OrderId, isExclusive: Boolean) =
      if (this.orderId == orderId)
        Left(AlreadyAcquiredByThisOrder)
      else
        Left(IsInUse)

    def release(orderId: OrderId) =
      if (this.orderId != orderId)
        Left(UnknownReleasingOrderError)
      else
        Right(Available)
  }

  final case class NonExclusiv(orderIds: Set[OrderId]) extends Acquired
  {
    assertThat(orderIds.nonEmpty)

    def lockCount = orderIds.size

    def isAcquiredBy(orderId: OrderId) =
      orderIds contains orderId

    def acquireFor(orderId: OrderId, isExclusive: Boolean) =
      if (orderIds contains orderId)
        Left(AlreadyAcquiredByThisOrder)
      else if (isExclusive)
        Left(IsInUse)
      else
        Right(NonExclusiv(orderIds + orderId))

    def release(orderId: OrderId) =
      if (!orderIds.contains(orderId))
        Left(UnknownReleasingOrderError)
      else
        Right(
          if (orderIds == Set(orderId))
            Available
          else
            copy(orderIds = orderIds - orderId))
  }

  sealed trait ReleaseError


  implicit val jsonCodec = TypedJsonCodec[Acquired](
    Subtype(deriveCodec[Exclusive]),
    Subtype(deriveCodec[NonExclusiv]),
    Subtype(Available))
}

