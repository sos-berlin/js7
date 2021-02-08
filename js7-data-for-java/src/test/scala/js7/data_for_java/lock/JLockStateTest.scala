package  js7.data_for_java.lock

import js7.data.lock.Acquired.{Available, Exclusive, NonExclusive}
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.order.OrderId
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.Queue

final class JLockStateTest extends AnyFreeSpec
{
  private val lock = Lock(LockId("LOCK"), limit = 1)
  private val availableLockState = JLockState(LockState(lock, Available))
  private val exclusiveLockState = JLockState(LockState(lock, Exclusive(OrderId("A")), Queue(OrderId("Q"))))
  private val nonExclusiveLockState = JLockState(LockState(lock, NonExclusive(Map(OrderId("A") -> 1)), Queue(OrderId("Q"))))

  "JLockState.isAvailable" in {
    JLockStateTester.testAvailable(availableLockState)
    JLockStateTester.testNotAvailable(exclusiveLockState)
    JLockStateTester.testNotAvailable(nonExclusiveLockState)
  }

  "JLockState.orderIds" in {
    assert(availableLockState.orderIds.isEmpty)
    JLockStateTester.testOrderIds(exclusiveLockState)
    JLockStateTester.testOrderIds(nonExclusiveLockState)
  }

  "JLockState.queuedOrderIds" in {
    assert(availableLockState.queuedOrderIds.isEmpty)
    JLockStateTester.testQueuedOrderIds(exclusiveLockState)
    JLockStateTester.testQueuedOrderIds(nonExclusiveLockState)
  }
}
