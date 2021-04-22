package js7.data.lock

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.event.KeyedEvent
import js7.data.item.ItemRevision
import js7.data.lock.Acquired.{Available, Exclusive, NonExclusive}
import js7.data.order.OrderEvent.{OrderLockAcquired, OrderLockEvent, OrderLockQueued, OrderLockReleased}
import js7.data.order.OrderId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.Queue

final class LockStateTest extends AnyFreeSpec
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

  def applyEvent(acquired: Acquired, event: KeyedEvent[OrderLockEvent])(implicit lock: Lock) =
    LockState(lock, acquired).applyEvent(event)

  def orderLockAcquired(orderId: OrderId, count: Option[Int])(implicit lock: Lock) =
    orderId <-: OrderLockAcquired(lock.path, count)

  for ((lock, testName) <- Seq(
    //Lock(LockPath("LOCK")) -> "Exclusive lock",
    Lock(LockPath("LOCK"), 1) -> "Lock with limit=1",
    Lock(LockPath("LOCK"), 3) -> "Lock with limit=3")) {

    implicit val implicitLock = lock

    testName - {
      "OrderLockAcquired" in {
        assert(applyEvent(Available, orderLockAcquired(a, None)) == Right(LockState(lock, Exclusive(a))))
        assert(applyEvent(Available, orderLockAcquired(a, Some(1))) == Right(LockState(lock, NonExclusive(Map(a -> 1)))))

        assert(applyEvent(Exclusive(a), orderLockAcquired(a, None)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), orderLockAcquired(b, None)) == Left(Problem("Lock:LOCK is in use")))

        assert(applyEvent(Exclusive(a), orderLockAcquired(a, Some(1))) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK is in use")))

        assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(a, None)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1)), orderLockAcquired(a, None)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusive(Map(b -> 1)), orderLockAcquired(a, None)) == Left(Problem("Lock:LOCK is in use")))

        assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1)), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusive(Map(b -> 1)), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK has already been acquired by this order")))
      }

      "OrderLockQueued" in {
        assert(applyEvent(Available, a <-: OrderLockQueued(lock.path, None)) == Left(Problem("Lock:LOCK is available an does not accept queuing")))

        assert(applyEvent(Exclusive(a), a <-: OrderLockQueued(lock.path, None)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), b <-: OrderLockQueued(lock.path, None)) == Right(LockState(lock, Exclusive(a), Queue(b))))

        assert(applyEvent(NonExclusive(Map(a -> 1)), a <-: OrderLockQueued(lock.path, None)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1)), a <-: OrderLockQueued(lock.path, None)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusive(Map(a -> 1)), b <-: OrderLockQueued(lock.path, None)) == Right(LockState(lock, NonExclusive(Map(a -> 1)), Queue(b))))

        assert(LockState(lock, Exclusive(a)).applyEvent(b <-: OrderLockQueued(lock.path, None)) == Right(LockState(lock, Exclusive(a), Queue(b))))
        assert(LockState(lock, Exclusive(a), Queue(b)).applyEvent(c <-: OrderLockQueued(lock.path, None)) == Right(LockState(lock, Exclusive(a), Queue(b, c))))
        assert(LockState(lock, Exclusive(a), Queue(b, c)).applyEvent(d <-: OrderLockQueued(lock.path, None)) == Right(LockState(lock, Exclusive(a), Queue(b, c, d))))
        assert(LockState(lock, Exclusive(a), Queue(b, c, d)).applyEvent(c <-: OrderLockQueued(lock.path, None)) == Left(Problem("Order 'C' already queues for Lock:LOCK")))
      }

      "Child order cannot lock if parent order has locked" in {
        assert(applyEvent(Exclusive(a), (a | "CHILD") <-: OrderLockQueued(lock.path, None)) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A")))
        assert(applyEvent(Exclusive(a), (a | "CHILD" | "GRANDCHILD") <-: OrderLockQueued(lock.path, None)) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A")))
        assert(applyEvent(Exclusive(a | "CHILD"), (a | "CHILD" | "GRANDCHILD") <-: OrderLockQueued(lock.path, None)) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A|CHILD")))
      }

      "OrderLockReleased" in {
        assert(applyEvent(Available, a <-: OrderLockReleased(lock.path)) == Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(applyEvent(Exclusive(a), a <-: OrderLockReleased(lock.path)) == Right(LockState(lock, Available)))
        assert(applyEvent(Exclusive(a), b <-: OrderLockReleased(lock.path)) == Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(applyEvent(NonExclusive(Map(a -> 1)), a <-: OrderLockReleased(lock.path)) == Right(LockState(lock, Available)))
        assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1)), a <-: OrderLockReleased(lock.path)) == Right(LockState(lock, NonExclusive(Map(b -> 1)))))
        assert(applyEvent(NonExclusive(Map(a -> 1)), b <-: OrderLockReleased(lock.path)) == Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1)), b <-: OrderLockReleased(lock.path)) == Right(LockState(lock, NonExclusive(Map(a -> 1)))))
        assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1, c -> 1)), b <-: OrderLockReleased(lock.path)) == Right(LockState(lock, NonExclusive(Map(a -> 1, c -> 1)))))
      }
    }
  }

  //"Exclusive lock used non-exclusively" in {
  //  implicit val lock = Lock(LockPath("LOCK"))
  //
  //  assert(applyEvent(Available, orderLockAcquired(a, None)) == Right(LockState(lock, Exclusive(a))))
  //  assert(applyEvent(Available, orderLockAcquired(a, Some(1))) == Right(LockState(lock, NonExclusive(Map(a -> 1)))))
  //  assert(applyEvent(Available, orderLockAcquired(a, Some(2))) == Left(Problem("Lock:LOCK: 0+2 would exceed limit=1")))
  //
  //  assert(applyEvent(Exclusive(a), orderLockAcquired(b, None)) == Left(Problem("Lock:LOCK is in use")))
  //  assert(applyEvent(Exclusive(a), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK is in use")))
  //  assert(applyEvent(Exclusive(a), orderLockAcquired(b, Some(2))) == Left(Problem("Lock:LOCK: Invalid count=2 requested")))
  //
  //  assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, None)) == Left(Problem("Lock:LOCK is in use")))
  //  assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK: 1+1 would exceed limit=1")))
  //  assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, Some(2))) == Left(Problem("Lock:LOCK: 1+2 would exceed limit=1")))
  //}

  "Lock with limit=3" - {
    implicit val lock = Lock(LockPath("LOCK"), limit = 3)

    "acquireFor" in {
      assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, Some(1))) == Right(LockState(lock, NonExclusive(Map(a -> 1, b -> 1)))))
      assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1)), orderLockAcquired(c, Some(1))) == Right(LockState(lock, NonExclusive(Map(a -> 1, b -> 1, c -> 1)))))
      assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1)), orderLockAcquired(c, None)) == Left(Problem("Lock:LOCK is in use")))
      assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1, c -> 1)), orderLockAcquired(d, Some(1))) == Left(Problem("Lock:LOCK: 3+1 would exceed limit=3")))
      assert(applyEvent(NonExclusive(Map(a -> 1, b -> 1, c -> 1)), orderLockAcquired(d, None)) == Left(Problem("Lock:LOCK is in use")))

      assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, Some(2))) == Right(LockState(lock, NonExclusive(Map(a -> 1, b -> 2)))))
      assert(applyEvent(NonExclusive(Map(a -> 1, b -> 2)), orderLockAcquired(c, Some(1))) == Left(Problem("Lock:LOCK: 3+1 would exceed limit=3")))
      assert(applyEvent(NonExclusive(Map(a -> 99)), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK: 99+1 would exceed limit=3")))
    }

    "acquireFor with invalid 'count' number" in {
      assert(applyEvent(Available, orderLockAcquired(c, Some(0))) == Left(Problem("Lock:LOCK: Invalid count=0 requested")))
      assert(applyEvent(Available, orderLockAcquired(c, Some(-1))) == Left(Problem("Lock:LOCK: Invalid count=-1 requested")))
      assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(c, Some(0))) == Left(Problem("Lock:LOCK: Invalid count=0 requested")))
      assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(c, Some(-1))) == Left(Problem("Lock:LOCK: Invalid count=-1 requested")))
    }
  }

  "acquireFor more than limit" in {
    implicit val lock = Lock(LockPath("LOCK"), limit = 1)
    assert(applyEvent(Available, a <-: OrderLockQueued(lock.path, Some(2))) ==
      Left(Problem("Cannot fulfill lock count=2 with Lock:LOCK limit=1")))
  }

  "Lock with limit=0" in {
    implicit val lock = Lock(LockPath("LOCK"), limit = 0)

      assert(applyEvent(Available, orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK: 0+1 would exceed limit=0")))
      assert(applyEvent(Exclusive(a), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK is in use")))
      assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, Some(1))) == Left(Problem("Lock:LOCK: 1+1 would exceed limit=0")))
  }

  "acquireFor with integer overflow" in {
    implicit val lock = Lock(LockPath("LOCK"), limit = Int.MaxValue)
    assert(applyEvent(NonExclusive(Map(a -> 1)), orderLockAcquired(b, Some(2))) == Right(LockState(lock, NonExclusive(Map(a -> 1, b -> 2)))))
    assert(Int.MaxValue + 1 == Int.MinValue)
    assert(Int.MaxValue + Int.MaxValue == -2)
    assert(applyEvent(NonExclusive(Map(a -> Int.MaxValue)), orderLockAcquired(b, Some(Int.MaxValue))) ==
      Left(Problem("Lock:LOCK: 2147483647+2147483647 would exceed limit=2147483647")))
  }
}
