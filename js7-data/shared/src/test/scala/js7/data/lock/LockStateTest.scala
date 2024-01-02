package js7.data.lock

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.item.ItemRevision
import js7.data.lock.Acquired.{Available, Exclusive, NonExclusive}
import js7.data.order.OrderId
import js7.tester.CirceJsonTester.testJson
import scala.collection.immutable.Queue

final class LockStateTest extends OurTestSuite
{
  "JSON" in {
    testJson(LockState(Lock(LockPath("LOCK"), limit = 1, Some(ItemRevision(0)))), json"""
      {
        "lock": {
          "path": "LOCK",
          "limit": 1,
          "itemRevision": 0
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

  // Similar to test of Acquired (see above), but with OrderLockEvent and limit check

  for ((lock, testName) <- Seq(
    //Lock(LockPath("LOCK")) -> "Exclusive lock",
    Lock(LockPath("LOCK"), 1) -> "Lock with limit=1",
    Lock(LockPath("LOCK"), 3) -> "Lock with limit=3")) {

    testName - {
      "OrderLockAcquired" in {
        assert(LockState(lock, Available).acquire(a, None) ==
          Right(LockState(lock, Exclusive(a))))
        assert(LockState(lock, Available).acquire(a, Some(1)) ==
          Right(LockState(lock, NonExclusive(Map(a -> 1)))))

        assert(LockState(lock, Exclusive(a)).acquire(a, None) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(LockState(lock, Exclusive(a)).acquire(b, None) ==
          Left(Problem("Lock:LOCK is in use")))

        assert(LockState(lock, Exclusive(a)).acquire(a, Some(1)) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(LockState(lock, Exclusive(a)).acquire(b, Some(1)) ==
          Left(Problem("Lock:LOCK is in use")))

        assert(LockState(lock, NonExclusive(Map(a -> 1))).acquire(a, None) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1))).acquire(a, None) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(LockState(lock, NonExclusive(Map(b -> 1))).acquire(a, None) ==
          Left(Problem("Lock:LOCK is in use")))

        assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1))).acquire(b, Some(1)) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(LockState(lock, NonExclusive(Map(b -> 1))).acquire(b, Some(1)) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
      }

      "OrderLockQueued" in {
        assert(LockState(lock, Available).enqueue(a, None) ==
          Right(LockState(lock, Available, Queue(a))))

        assert(LockState(lock, Exclusive(a)).enqueue(a, None) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(LockState(lock, Exclusive(a)).enqueue(b, None) ==
          Right(LockState(lock, Exclusive(a), Queue(b))))

        assert(LockState(lock, NonExclusive(Map(a -> 1))).enqueue(a, None) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1))).enqueue(a, None) ==
          Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(LockState(lock, NonExclusive(Map(a -> 1))).enqueue(b, None) ==
          Right(LockState(lock, NonExclusive(Map(a -> 1)), Queue(b))))

        assert(LockState(lock, Exclusive(a)).enqueue(b, None) ==
          Right(LockState(lock, Exclusive(a), Queue(b))))
        assert(LockState(lock, Exclusive(a), Queue(b)).enqueue(c, None) ==
          Right(LockState(lock, Exclusive(a), Queue(b, c))))
        assert(LockState(lock, Exclusive(a), Queue(b, c)).enqueue(d, None) ==
          Right(LockState(lock, Exclusive(a), Queue(b, c, d))))
        assert(LockState(lock, Exclusive(a), Queue(b, c, d)).enqueue(c, None) ==
          Left(Problem("Order:C already queues for Lock:LOCK")))
      }

      "Child order cannot lock if parent order has locked" in {
        assert(LockState(lock, Exclusive(a)).enqueue(a / "CHILD", None) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A")))
        assert(LockState(lock, Exclusive(a)).enqueue(a / "CHILD" / "GRANDCHILD", None) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A")))
        assert(LockState(lock, Exclusive(a / "CHILD")).enqueue(a / "CHILD" / "GRANDCHILD", None) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A|CHILD")))
      }

      "OrderLockReleased" in {
        assert(LockState(lock, Available).release(a) ==
          Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(LockState(lock, Exclusive(a)).release(a) ==
          Right(LockState(lock, Available)))
        assert(LockState(lock, Exclusive(a)).release(b) ==
          Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(LockState(lock, NonExclusive(Map(a -> 1))).release(a) ==
          Right(LockState(lock, Available)))
        assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1))).release(a) ==
          Right(LockState(lock, NonExclusive(Map(b -> 1)))))
        assert(LockState(lock, NonExclusive(Map(a -> 1))).release(b) ==
          Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1))).release(b) ==
          Right(LockState(lock, NonExclusive(Map(a -> 1)))))
        assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1, c -> 1))).release(b) ==
          Right(LockState(lock, NonExclusive(Map(a -> 1, c -> 1)))))
      }
    }
  }

  //"Exclusive lock used non-exclusively" in {
  //  implicit val lock = Lock(LockPath("LOCK"))
  //
  //  assert(applyEvent(Available, orderLockAcquired(a, None)) == Right(LockState(lock, Exclusive(a))))
  //  assert(applyEvent(Available, orderLockAcquired(a, Some(1))) == Right(LockState(lock, NonExclusive(Map(a -> 1)))))
  //  assert(applyEvent(Available, orderLockAcquired(a, Some(2))) == Left(Problem("Lock:LOCK: count=0 plus alreadyAcquired=2 would exceed limit=1")))
  //
  //  assert(applyEvent(Exclusive(a), orderLockAcquired(b, None)) == Left(Problem("Lock:LOCK is in use")))
  //  assert(applyEvent(Exclusive(a), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK is in use")))
  //  assert(applyEvent(Exclusive(a), orderLockAcquired(b, Some(2))) == Left(Problem("Lock:LOCK: Invalid count=2 requested")))
  //
  //  assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, None)) == Left(Problem("Lock:LOCK is in use")))
  //  assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK: count=1 plus alreadyAcquired=1 would exceed limit=1")))
  //  assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, Some(2))) == Left(Problem("Lock:LOCK: count=1 plus alreadyAcquired=2 would exceed limit=1")))
  //}

  "Lock with limit=3" - {
    val lock = Lock(LockPath("LOCK"), limit = 3)

    "acquire" in {
      assert(LockState(lock, NonExclusive(Map(a -> 1))).acquire(b, Some(1)) ==
        Right(LockState(lock, NonExclusive(Map(a -> 1, b -> 1)))))

      assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1))).acquire(c, Some(1)) ==
        Right(LockState(lock, NonExclusive(Map(a -> 1, b -> 1, c -> 1)))))

      assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1))).acquire(c, None) ==
        Left(Problem("Lock:LOCK is in use")))

      assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1, c -> 1))).acquire(d, Some(1)) ==
        Left(Problem("Lock:LOCK: count=1 plus alreadyAcquired=3 would exceed limit=3")))

      assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 1, c -> 1))).acquire(d, None) ==
        Left(Problem("Lock:LOCK is in use")))

      assert(LockState(lock, NonExclusive(Map(a -> 1))).acquire(b, Some(2)) ==
        Right(LockState(lock, NonExclusive(Map(a -> 1, b -> 2)))))

      assert(LockState(lock, NonExclusive(Map(a -> 1, b -> 2))).acquire(c, Some(1)) ==
        Left(Problem("Lock:LOCK: count=1 plus alreadyAcquired=3 would exceed limit=3")))

      assert(LockState(lock, NonExclusive(Map(a -> 99))).acquire(b, Some(1)) ==
        Left(Problem("Lock:LOCK: count=1 plus alreadyAcquired=99 would exceed limit=3")))
    }

    "acquire with invalid 'count' number" in {
      assert(LockState(lock, Available).acquire(c, Some(0)) ==
        Left(Problem("Lock:LOCK: Invalid count=0 requested")))

      assert(LockState(lock, Available).acquire(c, Some(-1)) ==
        Left(Problem("Lock:LOCK: Invalid count=-1 requested")))

      assert(LockState(lock, NonExclusive(Map(a -> 1))).acquire(c, Some(0)) ==
        Left(Problem("Lock:LOCK: Invalid count=0 requested")))

      assert(LockState(lock, NonExclusive(Map(a -> 1))).acquire(c, Some(-1)) ==
        Left(Problem("Lock:LOCK: Invalid count=-1 requested")))
    }

    "enqueue more than limit" in {
      assert(LockState(lock, Available).enqueue(a, Some(4)) ==
        Left(Problem("Cannot fulfill lock count=4 with Lock:LOCK limit=3")))
    }
  }

  "Lock with limit=0" in {
    val lock = Lock(LockPath("LOCK"), limit = 0)

    assert(LockState(lock, Available).acquire(b, Some(1)) ==
      Left(Problem("Lock:LOCK: count=1 plus alreadyAcquired=0 would exceed limit=0")))

    assert(LockState(lock, Exclusive(a)).acquire(b, Some(1)) ==
      Left(Problem("Lock:LOCK is in use")))

    assert(LockState(lock, NonExclusive(Map(a -> 1))).acquire(b, Some(1)) ==
      Left(Problem("Lock:LOCK: count=1 plus alreadyAcquired=1 would exceed limit=0")))
  }

  "acquire with integer overflow" in {
    implicit val lock = Lock(LockPath("LOCK"), limit = Int.MaxValue)
    assert(LockState(lock, NonExclusive(Map(a -> 1))).acquire(b, Some(2)) ==
      Right(LockState(lock, NonExclusive(Map(a -> 1, b -> 2)))))
    assert(Int.MaxValue + 1 == Int.MinValue)
    assert(Int.MaxValue + Int.MaxValue == -2)
    assert(LockState(lock, NonExclusive(Map(a -> Int.MaxValue))).acquire(b, Some(Int.MaxValue)) ==
      Left(Problem("Lock:LOCK: count=2147483647 plus alreadyAcquired=2147483647 would exceed limit=2147483647")))
  }
}
