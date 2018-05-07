package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.event.collector.SyncExclusiveTest._
import com.sos.jobscheduler.common.event.{EventIdGenerator, Sync}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventId, NoKeyEvent, Stamped}
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class SyncExclusiveTest extends FreeSpec {

  "test" in {
    val queue = new MemoryKeyedEventQueue(sizeLimit = 100)
    autoClosing(TimerService()) { timerService ⇒
      val sync = new Sync(initialLastEventId = EventId.BeforeFirst, timerService)
      for ((aEventId, bEventId) ← List((1L, 2L), (3L, 4L), (5L, 6L))) {
        val a = sync.whenEventIsAvailable(aEventId, now + 1.hour)
        assert(a ne sync.whenEventIsAvailable(aEventId, now + 1.hour))
        assert(!a.isCompleted)
        val b = sync.whenEventIsAvailable(bEventId, now + 1.hour)
        assert(!b.isCompleted)
        queue.add(Stamped(aEventId, Timestamp.ofEpochMilli(0), TestEvent))
        sync.onEventAdded(aEventId)
        a await 1.s
        assert(a.isCompleted)
        assert(a.successValue)
        b await 1.ms  // b is completed, too, because Sync only waits for the next event. Sync does not wait for events in the far future
        assert(!sync.whenEventIsAvailable(aEventId, now + 1.hour).isCompleted)
        assert(!sync.whenEventIsAvailable(aEventId, now + 1.hour).isCompleted)
      }
      waitForCondition(1.s, 10.ms) { timerService.queueSize == 3 }  // One open Timer per EventId
      assert(timerService.queueSize == 3)
    }
  }

  "timeout" in {
    val queue = new MemoryKeyedEventQueue(sizeLimit = 100)
    autoClosing(TimerService()) { timerService ⇒
      val sync = new Sync(initialLastEventId = EventId.BeforeFirst, timerService)
      for (eventId ← 1L to 3L) {
        val a = sync.whenEventIsAvailable(eventId, now + 200.milliseconds)
        val b = sync.whenEventIsAvailable(eventId, now + 1.hour)
        assert(a ne b)
        assert(!a.isCompleted)
        a await 400.ms
        assert(!a.successValue)  // false: Timed out
        assert(!b.isCompleted)
        queue.add(Stamped(eventId, Timestamp.ofEpochMilli(0), TestEvent))
        sync.onEventAdded(eventId)
        b await 1.s
        assert(b.isCompleted)
        assert(b.successValue)  // true: Event arrived
      }
      assert(waitForCondition(1.s, 10.ms) { timerService.isEmpty })
    }
  }

  if (sys.props contains "test.speed") "speed" in {
    autoClosing(TimerService()) { timerService ⇒
      val sync = new Sync(initialLastEventId = EventId.BeforeFirst, timerService)
      val n = 10000
      val eventIdGenerator = new EventIdGenerator
      for (_ ← 1 to 10) {
        val stopwatch = new Stopwatch
        val eventIds = for (_ ← 1 to n) yield eventIdGenerator.next()
        val futures: Seq[Future[Boolean]] = (for (eventId ← eventIds) yield Future { sync.whenEventIsAvailable(after = eventId - 1, now + 99.seconds) }) map { _.flatten }
        eventIds foreach sync.onEventAdded
        val result = futures await 99.s
        assert(result forall identity)
        info(stopwatch.itemsPerSecondString(n, "events"))
      }
    }
  }
}

object SyncExclusiveTest {
  private object TestEvent extends NoKeyEvent
}
