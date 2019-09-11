package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.event.{EventIdGenerator, Sync}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.event.EventId
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class SyncExclusiveTest extends FreeSpec {

  "test" in {
    val sync = new Sync(initial = EventId.BeforeFirst, EventId.toString)
    for ((aEventId, bEventId) <- List((1L, 2L), (3L, 4L), (5L, 6L))) {
      val a = sync.whenAvailable(aEventId, until = None).runToFuture
      assert(!a.isCompleted)
      val b = sync.whenAvailable(bEventId, until = None).runToFuture
      assert(!b.isCompleted)
      sync.onAdded(aEventId)
      a await 99.s
      assert(a.isCompleted)
      assert(a.successValue)
      b await 99.s  // b is completed, too, because Sync only waits for the next event. Sync does not wait for events in the far future
      assert(!sync.whenAvailable(aEventId, until = None).runToFuture.isCompleted)
      assert(!sync.whenAvailable(aEventId, until = None).runToFuture.isCompleted)
    }
  }

  "timeout" in {
    val tick = 200.millisecond
    val sync = new Sync(initial = EventId.BeforeFirst, EventId.toString)
    for (eventId <- 1L to 3L) {
      withClue(s"#$eventId") {
        val a = sync.whenAvailable(eventId, until = Some(now + 2*tick), delay = 2*tick).runToFuture
        val b = sync.whenAvailable(eventId, until = Some(now + 1.hour), delay = 2*tick).runToFuture
        assert(a ne b)

        sleep(tick)
        assert(!a.isCompleted)

        a await 2*tick            // `until` elapsed
        assert(!a.successValue)   // false: Timed out
        assert(!b.isCompleted)

        sync.onAdded(eventId)
        assert(!b.isCompleted)    // Still delayed

        sleep(tick)
        assert(!b.isCompleted)    // Still delayed

        b await 99.seconds
        assert(b.isCompleted)
        assert(b.successValue)    // true: Event arrived
      }
    }
  }

  if (sys.props contains "test.speed") "speed" in {
    val sync = new Sync(initial = EventId.BeforeFirst, EventId.toString)
    val n = 10000
    val eventIdGenerator = new EventIdGenerator
    for (_ <- 1 to 10) {
      val stopwatch = new Stopwatch
      val eventIds = for (_ <- 1 to n) yield eventIdGenerator.next()
      val futures: Seq[Future[Boolean]] =
        (for (eventId <- eventIds) yield Future { sync.whenAvailable(after = eventId - 1, Some(now + 99.seconds)).runToFuture })
          .map({ _.flatten })
      eventIds foreach sync.onAdded
      val result = futures await 99.s
      assert(result forall identity)
      info(stopwatch.itemsPerSecondString(n, "events"))
    }
  }
}
