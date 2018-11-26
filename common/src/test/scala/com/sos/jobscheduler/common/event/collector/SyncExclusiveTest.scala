package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.event.{EventIdGenerator, Sync}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.EventId
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class SyncExclusiveTest extends FreeSpec {

  "test" in {
    val sync = new Sync(initialLastEventId = EventId.BeforeFirst)
    for ((aEventId, bEventId) ← List((1L, 2L), (3L, 4L), (5L, 6L))) {
      val a = sync.whenEventIsAvailable(aEventId, until = now + 1.hour).runAsync
      assert(!a.isCompleted)
      val b = sync.whenEventIsAvailable(bEventId, until = now + 1.hour).runAsync
      assert(!b.isCompleted)
      sync.onEventAdded(aEventId)
      a await 1.s
      assert(a.isCompleted)
      assert(a.successValue)
      b await 1.ms  // b is completed, too, because Sync only waits for the next event. Sync does not wait for events in the far future
      assert(!sync.whenEventIsAvailable(aEventId, until = now + 1.hour).runAsync.isCompleted)
      assert(!sync.whenEventIsAvailable(aEventId, until = now + 1.hour).runAsync.isCompleted)
    }
  }

  "timeout" in {
    val sync = new Sync(initialLastEventId = EventId.BeforeFirst)
    for (eventId ← 1L to 3L) {
      val a = sync.whenEventIsAvailable(eventId, until = now + 400.milliseconds, delay = 200.milliseconds).runAsync
      val b = sync.whenEventIsAvailable(eventId, until = now + 1.hour          , delay = 200.milliseconds).runAsync
      assert(a ne b)

      sleep(100.ms)
      assert(!a.isCompleted)

      a await 500.ms
      assert(!a.successValue)  // false: Timed out
      assert(!b.isCompleted)

      sync.onEventAdded(eventId)
      assert(!b.isCompleted)

      sleep(100.milliseconds)
      assert(!b.isCompleted)

      sleep(200.milliseconds)
      assert(b.isCompleted)
      assert(b.successValue)  // true: Event arrived
    }
  }

  if (sys.props contains "test.speed") "speed" in {
    val sync = new Sync(initialLastEventId = EventId.BeforeFirst)
    val n = 10000
    val eventIdGenerator = new EventIdGenerator
    for (_ ← 1 to 10) {
      val stopwatch = new Stopwatch
      val eventIds = for (_ ← 1 to n) yield eventIdGenerator.next()
      val futures: Seq[Future[Boolean]] = (for (eventId ← eventIds) yield Future { sync.whenEventIsAvailable(after = eventId - 1, now + 99.seconds).runAsync }) map { _.flatten }
      eventIds foreach sync.onEventAdded
      val result = futures await 99.s
      assert(result forall identity)
      info(stopwatch.itemsPerSecondString(n, "events"))
    }
  }
}
