package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.event.collector.SyncTest._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.event.{NoKeyEvent, Snapshot}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SyncTest extends FreeSpec {

  "test" in {
    val queue = new KeyedEventQueue(sizeLimit = 100)
    autoClosing(TimerService()) { timerService ⇒
      val sync = new Sync(timerService)
      for (eventId ← 1L to 3L) {
        val a = sync.whenEventIsAvailable(eventId, 99999.s)
        assert(a ne sync.whenEventIsAvailable(eventId, 99999.s))
        assert(!a.isCompleted)
        queue.add(Snapshot(eventId, TestEvent))
        sync.onNewEvent(eventId)
        a await 1.s
        assert(a.isCompleted)
        assert(a.successValue)
        assert(!sync.whenEventIsAvailable(eventId, 99999.s).isCompleted)
        assert(!sync.whenEventIsAvailable(eventId, 99999.s).isCompleted)
      }
      assert(waitForCondition(1.s, 10.ms) { timerService.queueSize == 3 })  // One open Timer per EventId
    }
  }

  "timeout" in {
    val queue = new KeyedEventQueue(sizeLimit = 100)
    autoClosing(TimerService()) { timerService ⇒
      val sync = new Sync(timerService)
      for (eventId ← 1L to 3L) {
        val a = sync.whenEventIsAvailable(eventId, 200.ms)
        val b = sync.whenEventIsAvailable(eventId, 99999.s)
        assert(a ne b)
        assert(!a.isCompleted)
        a await 400.ms
        assert(!a.successValue)  // false: Timed out
        assert(!b.isCompleted)
        queue.add(Snapshot(eventId, TestEvent))
        sync.onNewEvent(eventId)
        b await 1.s
        assert(b.isCompleted)
        assert(b.successValue)  // true: Event arrived
      }
      assert(waitForCondition(1.s, 10.ms) { timerService.isEmpty })
    }
  }
}

object SyncTest {
  private object TestEvent extends NoKeyEvent
}
