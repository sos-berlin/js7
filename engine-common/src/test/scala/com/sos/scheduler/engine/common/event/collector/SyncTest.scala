package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.common.event.collector.SyncTest._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.{SuccessFuture, _}
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.event.{NoKeyEvent, Snapshot}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SyncTest extends FreeSpec {

  "test" in {
    val queue = new KeyedEventQueue(sizeLimit = 100)
    autoClosing(TimerService()) { timerService ⇒
      val sync = new Sync(timerService)
      for (List(aEventId, bEventId) ← List(List(1L, 2L), List(3L, 4L), List(5L, 6L))) {
        val a = sync.whenEventIsAvailable(aEventId, 99999.s)
        assert(a ne sync.whenEventIsAvailable(aEventId, 99999.s))
        assert(!a.isCompleted)
        val b = sync.whenEventIsAvailable(bEventId, 99999.s)
        assert(!b.isCompleted)
        queue.add(Snapshot(aEventId, TestEvent))
        sync.onNewEvent(aEventId)
        a await 1.s
        assert(a.isCompleted)
        assert(a.successValue)
        b await 1.ms  // b is completed, too, because Sync only waits for the next event. Sync does not wait for events in the far future
        assert(!sync.whenEventIsAvailable(aEventId, 99999.s).isCompleted)
        assert(!sync.whenEventIsAvailable(aEventId, 99999.s).isCompleted)
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

  if (sys.props contains "test.speed") "speed" in {
    autoClosing(TimerService()) { timerService ⇒
      val sync = new Sync(timerService)
      val n = 10000
      val eventIdGenerator = new EventIdGenerator
      for (_ ← 1 to 10) {
        val stopwatch = new Stopwatch
        val eventIds = for (_ ← 1 to n) yield eventIdGenerator.next()
        val futures: Seq[Future[Boolean]] = (for (eventId ← eventIds) yield Future { sync.whenEventIsAvailable(after = eventId - 1, 99.s) }) map { _.flatten }
        eventIds foreach sync.onNewEvent
        val result = futures await 99.s
        assert(result forall identity)
        logger.info(stopwatch.itemsPerSecondString(n, "event"))
      }
    }
  }
}

object SyncTest {
  private val logger = Logger(getClass)
  private object TestEvent extends NoKeyEvent
}
