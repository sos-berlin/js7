package js7.data.lock

import js7.base.circeutils.CirceUtils._
import js7.data.lock.Acquired.{Available, Exclusive, NonExclusive}
import js7.data.order.OrderId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AcquiredTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(LockState(Lock(LockId("LOCK"))), json"""
      {
        "lock": {
          "id": "LOCK"
        },
        "acquired": {
          "TYPE": "Available"
        },
        "queue": []
      }""")
  }

  private val a = OrderId("A")
  private val b = OrderId("B")
  private val c = OrderId("C")
  private val d = OrderId("D")

  "Acquired" - {
    "acquireFor" in {
      assert(Available.acquireFor(a, None) == Right(Exclusive(a)))
      assert(Available.acquireFor(a, Some(1)) == Right(NonExclusive(Map(a -> 1))))

      assert(Exclusive(a).acquireFor(a, None) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(Exclusive(a).acquireFor(b, None) == Left(LockRefusal.IsInUse))

      assert(Exclusive(a).acquireFor(a, Some(1)) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(Exclusive(a).acquireFor(b, Some(1)) == Left(LockRefusal.IsInUse))

      assert(NonExclusive(Map(a -> 1)).acquireFor(a, None) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(NonExclusive(Map(a -> 1, b -> 1)).acquireFor(a, None) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(NonExclusive(Map(b -> 1)).acquireFor(a, None) == Left(LockRefusal.IsInUse))

      assert(NonExclusive(Map(a -> 1)).acquireFor(b, Some(1)) == Right(NonExclusive(Map(a -> 1, b -> 1))))
      assert(NonExclusive(Map(a -> 1, b -> 1)).acquireFor(b, Some(1)) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(NonExclusive(Map(b -> 1)).acquireFor(b, Some(1)) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(NonExclusive(Map(a -> 1, b -> 22)).acquireFor(c, Some(333)) == Right(NonExclusive(Map(a -> 1, b -> 22, c -> 333))))
    }

    "release" in {
      assert(Available.release(a) == Left(LockRefusal.UnknownReleasingOrderError))

      assert(Exclusive(a).release(a) == Right(Available))
      assert(Exclusive(a).release(b) == Left(LockRefusal.UnknownReleasingOrderError))

      assert(NonExclusive(Map(a -> 1)).release(a) == Right(Available))
      assert(NonExclusive(Map(a -> 1, b -> 1)).release(a) == Right(NonExclusive(Map(b -> 1))))
      assert(NonExclusive(Map(a -> 1)).release(b) == Left(LockRefusal.UnknownReleasingOrderError))

      assert(NonExclusive(Map(a -> 1, b -> 1)).release(b) == Right(NonExclusive(Map(a -> 1))))
      assert(NonExclusive(Map(a -> 1, b -> 1, c -> 1)).release(b) == Right(NonExclusive(Map(a -> 1, c -> 1))))
    }
  }
}
