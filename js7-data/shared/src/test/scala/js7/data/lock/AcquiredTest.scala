package js7.data.lock

import js7.base.circeutils.CirceUtils._
import js7.data.lock.Acquired.{Exclusive, Available, NonExclusiv}
import js7.data.order.OrderId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AcquiredTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(LockState(Lock(LockName("LOCK"))), json"""
      {
        "lock": {
          "name": "LOCK"
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
      assert(Available.acquireFor(a, isExclusive = true) == Right(Exclusive(a)))
      assert(Available.acquireFor(a, isExclusive = false) == Right(NonExclusiv(Set(a))))

      assert(Exclusive(a).acquireFor(a, isExclusive = true) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(Exclusive(a).acquireFor(b, isExclusive = true) == Left(LockRefusal.IsInUse))

      assert(Exclusive(a).acquireFor(a, isExclusive = false) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(Exclusive(a).acquireFor(b, isExclusive = false) == Left(LockRefusal.IsInUse))

      assert(NonExclusiv(Set(a)).acquireFor(a, isExclusive = true) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(NonExclusiv(Set(a, b)).acquireFor(a, isExclusive = true) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(NonExclusiv(Set(b)).acquireFor(a, isExclusive = true) == Left(LockRefusal.IsInUse))

      assert(NonExclusiv(Set(a)).acquireFor(b, isExclusive = false) == Right(NonExclusiv(Set(a, b))))
      assert(NonExclusiv(Set(a, b)).acquireFor(b, isExclusive = false) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(NonExclusiv(Set(b)).acquireFor(b, isExclusive = false) == Left(LockRefusal.AlreadyAcquiredByThisOrder))
      assert(NonExclusiv(Set(a, b)).acquireFor(c, isExclusive = false) == Right(NonExclusiv(Set(a, b, c))))
    }

    "release" in {
      assert(Available.release(a) == Left(LockRefusal.UnknownReleasingOrderError))

      assert(Exclusive(a).release(a) == Right(Available))
      assert(Exclusive(a).release(b) == Left(LockRefusal.UnknownReleasingOrderError))

      assert(NonExclusiv(Set(a)).release(a) == Right(Available))
      assert(NonExclusiv(Set(a, b)).release(a) == Right(NonExclusiv(Set(b))))
      assert(NonExclusiv(Set(a)).release(b) == Left(LockRefusal.UnknownReleasingOrderError))

      assert(NonExclusiv(Set(a, b)).release(b) == Right(NonExclusiv(Set(a))))
      assert(NonExclusiv(Set(a, b, c)).release(b) == Right(NonExclusiv(Set(a, c))))
    }
  }
}
