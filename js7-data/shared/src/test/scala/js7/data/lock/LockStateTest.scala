package js7.data.lock

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.event.KeyedEvent
import js7.data.lock.Acquired.{Exclusive, Available, NonExclusiv}
import js7.data.order.OrderEvent.{OrderLockAcquired, OrderLockEvent, OrderLockQueued, OrderLockReleased}
import js7.data.order.OrderId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.Queue

final class LockStateTest extends AnyFreeSpec
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

  // Similar to test of Acquired (see above), but with OrderLockEvent and limit check

  def applyEvent(acquired: Acquired, event: KeyedEvent[OrderLockEvent])(implicit lock: Lock) =
    LockState(lock, acquired).applyEvent(event)

  def orderLockAcquired(orderId: OrderId, isExclusive: Boolean)(implicit lock: Lock) =
    orderId <-: OrderLockAcquired(lock.name, exclusively = isExclusive)

  for ((lock, testName) <- Seq(
    Lock(LockName("LOCK")) -> "Exclusive lock",
    Lock(LockName("LOCK"), Some(1)) -> "Non-exclusive with limit=1",
    Lock(LockName("LOCK"), Some(3)) -> "Non-exclusive with limit=3")) {

    implicit val implicitLock = lock

    testName - {
      "OrderLockAcquired" in {
        assert(applyEvent(Available, orderLockAcquired(a, isExclusive = true)) == Right(LockState(lock, Exclusive(a))))
        assert(applyEvent(Available, orderLockAcquired(a, isExclusive = false)) == Right(LockState(lock, NonExclusiv(Set(a)))))

        assert(applyEvent(Exclusive(a), orderLockAcquired(a, isExclusive = true)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), orderLockAcquired(b, isExclusive = true)) == Left(Problem("Lock 'LOCK' is in use")))

        assert(applyEvent(Exclusive(a), orderLockAcquired(a, isExclusive = false)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), orderLockAcquired(b, isExclusive = false)) == Left(Problem("Lock 'LOCK' is in use")))

        assert(applyEvent(NonExclusiv(Set(a)), orderLockAcquired(a, isExclusive = true)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(a, b)), orderLockAcquired(a, isExclusive = true)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(b)), orderLockAcquired(a, isExclusive = true)) == Left(Problem("Lock 'LOCK' is in use")))

        assert(applyEvent(NonExclusiv(Set(a, b)), orderLockAcquired(b, isExclusive = false)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(b)), orderLockAcquired(b, isExclusive = false)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
      }

      "OrderLockQueued" in {
        assert(applyEvent(Available, a <-: OrderLockQueued(lock.name)) == Left(Problem("Lock 'LOCK' is available an does not accept queuing")))

        assert(applyEvent(Exclusive(a), a <-: OrderLockQueued(lock.name)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
        assert(applyEvent(Exclusive(a), b <-: OrderLockQueued(lock.name)) == Right(LockState(lock, Exclusive(a), Queue(b))))

        assert(applyEvent(NonExclusiv(Set(a)), a <-: OrderLockQueued(lock.name)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(a, b)), a <-: OrderLockQueued(lock.name)) == Left(Problem("Lock 'LOCK' has already been acquired by this order")))
        assert(applyEvent(NonExclusiv(Set(a)), b <-: OrderLockQueued(lock.name)) == Right(LockState(lock, NonExclusiv(Set(a)), Queue(b))))

        assert(LockState(lock, Exclusive(a)).applyEvent(b <-: OrderLockQueued(lock.name)) == Right(LockState(lock, Exclusive(a), Queue(b))))
        assert(LockState(lock, Exclusive(a), Queue(b)).applyEvent(c <-: OrderLockQueued(lock.name)) == Right(LockState(lock, Exclusive(a), Queue(b, c))))
        assert(LockState(lock, Exclusive(a), Queue(b, c)).applyEvent(d <-: OrderLockQueued(lock.name)) == Right(LockState(lock, Exclusive(a), Queue(b, c, d))))
        assert(LockState(lock, Exclusive(a), Queue(b, c, d)).applyEvent(c <-: OrderLockQueued(lock.name)) == Left(Problem("Order 'C' already queues for Lock 'LOCK'")))
      }

      "OrderLockReleased" in {
        assert(applyEvent(Available, a <-: OrderLockReleased(lock.name)) == Left(Problem("Lock 'LOCK' has not been acquired by this order")))

        assert(applyEvent(Exclusive(a), a <-: OrderLockReleased(lock.name)) == Right(LockState(lock, Available)))
        assert(applyEvent(Exclusive(a), b <-: OrderLockReleased(lock.name)) == Left(Problem("Lock 'LOCK' has not been acquired by this order")))

        assert(applyEvent(NonExclusiv(Set(a)), a <-: OrderLockReleased(lock.name)) == Right(LockState(lock, Available)))
        assert(applyEvent(NonExclusiv(Set(a, b)), a <-: OrderLockReleased(lock.name)) == Right(LockState(lock, NonExclusiv(Set(b)))))
        assert(applyEvent(NonExclusiv(Set(a)), b <-: OrderLockReleased(lock.name)) == Left(Problem("Lock 'LOCK' has not been acquired by this order")))

        assert(applyEvent(NonExclusiv(Set(a, b)), b <-: OrderLockReleased(lock.name)) == Right(LockState(lock, NonExclusiv(Set(a)))))
        assert(applyEvent(NonExclusiv(Set(a, b, c)), b <-: OrderLockReleased(lock.name)) == Right(LockState(lock, NonExclusiv(Set(a, c)))))
      }
    }
  }

  "Non-exclusive lock with limit=3" - {
    implicit val lock = Lock(LockName("LOCK"), nonExclusiveLimit = Some(3))

    "acquireFor" in {
      assert(applyEvent(NonExclusiv(Set(a)), orderLockAcquired(b, isExclusive = false)) == Right(LockState(lock, NonExclusiv(Set(a, b)))))
      assert(applyEvent(NonExclusiv(Set(a, b)), orderLockAcquired(c, isExclusive = false)) == Right(LockState(lock, NonExclusiv(Set(a, b, c)))))
      assert(applyEvent(NonExclusiv(Set(a, b)), orderLockAcquired(c, isExclusive = true)) == Left(Problem("Lock 'LOCK' is in use")))
      assert(applyEvent(NonExclusiv(Set(a, b, c)), orderLockAcquired(d, isExclusive = false)) == Left(Problem("Lock 'LOCK' limit=3 reached")))
      assert(applyEvent(NonExclusiv(Set(a, b, c)), orderLockAcquired(d, isExclusive = true)) == Left(Problem("Lock 'LOCK' is in use")))
    }
  }
}
