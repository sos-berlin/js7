package js7.data.lock

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.event.KeyedEvent
import js7.data.lock.Acquired.{Available, Exclusive, NonExclusiv}
import js7.data.order.OrderEvent.{OrderLockAcquired, OrderLockEvent, OrderLockQueued, OrderLockReleased}
import js7.data.order.OrderId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.Queue

final class LockStateTest extends AnyFreeSpec
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

  // Similar to test of Acquired (see above), but with OrderLockEvent and limit check

  def applyEvent(acquired: Acquired, event: KeyedEvent[OrderLockEvent])(implicit lock: Lock) =
    LockState(lock, acquired).applyEvent(event)

  def orderLockAcquired(orderId: OrderId, isExclusive: Boolean)(implicit lock: Lock) =
    orderId <-: OrderLockAcquired(lock.id, exclusively = isExclusive)

  for ((lock, testName) <- Seq(
    Lock(LockId("LOCK")) -> "Exclusive lock",
    Lock(LockId("LOCK"), Some(1)) -> "Non-exclusive with limit=1",
    Lock(LockId("LOCK"), Some(3)) -> "Non-exclusive with limit=3")) {

    implicit val implicitLock = lock

    testName - {
      "OrderLockAcquired" in {
        assert(applyEvent(Available, orderLockAcquired(a, isExclusive = true)) == Right(LockState(lock, Exclusive(a))))
        assert(applyEvent(Available, orderLockAcquired(a, isExclusive = false)) == Right(LockState(lock, NonExclusiv(Set(a)))))

        assert(applyEvent(Exclusive(a), orderLockAcquired(a, isExclusive = true)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), orderLockAcquired(b, isExclusive = true)) == Left(Problem("Lock:LOCK is in use")))

        assert(applyEvent(Exclusive(a), orderLockAcquired(a, isExclusive = false)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), orderLockAcquired(b, isExclusive = false)) == Left(Problem("Lock:LOCK is in use")))

        assert(applyEvent(NonExclusiv(Set(a)), orderLockAcquired(a, isExclusive = true)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(a, b)), orderLockAcquired(a, isExclusive = true)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(b)), orderLockAcquired(a, isExclusive = true)) == Left(Problem("Lock:LOCK is in use")))

        assert(applyEvent(NonExclusiv(Set(a, b)), orderLockAcquired(b, isExclusive = false)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(b)), orderLockAcquired(b, isExclusive = false)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
      }

      "OrderLockQueued" in {
        assert(applyEvent(Available, a <-: OrderLockQueued(lock.id)) == Left(Problem("Lock:LOCK is available an does not accept queuing")))

        assert(applyEvent(Exclusive(a), a <-: OrderLockQueued(lock.id)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), b <-: OrderLockQueued(lock.id)) == Right(LockState(lock, Exclusive(a), Queue(b))))

        assert(applyEvent(NonExclusiv(Set(a)), a <-: OrderLockQueued(lock.id)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(a, b)), a <-: OrderLockQueued(lock.id)) == Left(Problem("Lock:LOCK has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(a)), b <-: OrderLockQueued(lock.id)) == Right(LockState(lock, NonExclusiv(Set(a)), Queue(b))))

        assert(LockState(lock, Exclusive(a)).applyEvent(b <-: OrderLockQueued(lock.id)) == Right(LockState(lock, Exclusive(a), Queue(b))))
        assert(LockState(lock, Exclusive(a), Queue(b)).applyEvent(c <-: OrderLockQueued(lock.id)) == Right(LockState(lock, Exclusive(a), Queue(b, c))))
        assert(LockState(lock, Exclusive(a), Queue(b, c)).applyEvent(d <-: OrderLockQueued(lock.id)) == Right(LockState(lock, Exclusive(a), Queue(b, c, d))))
        assert(LockState(lock, Exclusive(a), Queue(b, c, d)).applyEvent(c <-: OrderLockQueued(lock.id)) == Left(Problem("Order 'C' already queues for Lock:LOCK")))
      }

      "Child order cannot lock if parent order has locked" in {
        assert(applyEvent(Exclusive(a), (a | "CHILD") <-: OrderLockQueued(lock.id)) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A")))
        assert(applyEvent(Exclusive(a), (a | "CHILD" | "GRANDCHILD") <-: OrderLockQueued(lock.id)) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A")))
        assert(applyEvent(Exclusive(a | "CHILD"), (a | "CHILD" | "GRANDCHILD") <-: OrderLockQueued(lock.id)) ==
          Left(Problem("Lock:LOCK has already been acquired by parent Order:A|CHILD")))
      }

      "OrderLockReleased" in {
        assert(applyEvent(Available, a <-: OrderLockReleased(lock.id)) == Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(applyEvent(Exclusive(a), a <-: OrderLockReleased(lock.id)) == Right(LockState(lock, Available)))
        assert(applyEvent(Exclusive(a), b <-: OrderLockReleased(lock.id)) == Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(applyEvent(NonExclusiv(Set(a)), a <-: OrderLockReleased(lock.id)) == Right(LockState(lock, Available)))
        assert(applyEvent(NonExclusiv(Set(a, b)), a <-: OrderLockReleased(lock.id)) == Right(LockState(lock, NonExclusiv(Set(b)))))
        assert(applyEvent(NonExclusiv(Set(a)), b <-: OrderLockReleased(lock.id)) == Left(Problem("Lock:LOCK has not been acquired by this order")))

        assert(applyEvent(NonExclusiv(Set(a, b)), b <-: OrderLockReleased(lock.id)) == Right(LockState(lock, NonExclusiv(Set(a)))))
        assert(applyEvent(NonExclusiv(Set(a, b, c)), b <-: OrderLockReleased(lock.id)) == Right(LockState(lock, NonExclusiv(Set(a, c)))))
      }
    }
  }

  "Non-exclusive lock with limit=3" - {
    implicit val lock = Lock(LockId("LOCK"), nonExclusiveLimit = Some(3))

    "acquireFor" in {
      assert(applyEvent(NonExclusiv(Set(a)), orderLockAcquired(b, isExclusive = false)) == Right(LockState(lock, NonExclusiv(Set(a, b)))))
      assert(applyEvent(NonExclusiv(Set(a, b)), orderLockAcquired(c, isExclusive = false)) == Right(LockState(lock, NonExclusiv(Set(a, b, c)))))
      assert(applyEvent(NonExclusiv(Set(a, b)), orderLockAcquired(c, isExclusive = true)) == Left(Problem("Lock:LOCK is in use")))
      assert(applyEvent(NonExclusiv(Set(a, b, c)), orderLockAcquired(d, isExclusive = false)) == Left(Problem("Lock:LOCK limit=3 reached")))
      assert(applyEvent(NonExclusiv(Set(a, b, c)), orderLockAcquired(d, isExclusive = true)) == Left(Problem("Lock:LOCK is in use")))
    }
  }
}
