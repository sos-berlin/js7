package js7.common.event.collector

import js7.base.monixutils.MonixDeadline.now
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.common.event.{EventIdGenerator, EventSync}
import js7.common.scalautil.Futures.implicits._
import js7.data.event.EventId
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class EventSyncTest extends AnyFreeSpec
{
  "test" in {
    implicit val scheduler = TestScheduler()

    val sync = new EventSync(initial = EventId.BeforeFirst, EventId.toString)
    var waitingCount = 0
    for ((aEventId, bEventId, cEventId) <- List((1L, 2L, 3L), (4L, 5L, 6L), (7L, 8L, 9L))) {
      val a = sync.whenAvailable(aEventId, until = None).runToFuture
      val b = sync.whenAvailable(bEventId, until = None).runToFuture
      val c = sync.whenAvailable(cEventId, until = None).runToFuture

      sync.onAdded(aEventId)
      scheduler.tick()
      assert(!a.isCompleted)
      assert(!b.isCompleted)

      sync.onAdded(bEventId)
      scheduler.tick()
      assert(a.isCompleted)
      assert(a.successValue)
      assert(!b.isCompleted)

      sync.onAdded(cEventId)
      scheduler.tick()
      assert(!c.isCompleted)

      assert(sync.whenAvailable(aEventId, until = None).runToFuture.isCompleted)
      assert(sync.whenAvailable(bEventId, until = None).runToFuture.isCompleted)
      assert(!sync.whenAvailable(cEventId, until = None).runToFuture.isCompleted)

      assert(sync.waitingCount == 1)  // The last whenAvailable has not yet completed (in each test loop iteration)
      waitingCount = sync.waitingCount
    }
  }

  "timeout" in {
    implicit val scheduler = TestScheduler()
    val tick = 1.s
    val sync = new EventSync(initial = EventId.BeforeFirst, EventId.toString)
    for (eventId <- 1L to 3L) {
      withClue(s"#$eventId") {
        val a = sync.whenAvailable(eventId - 1, until = Some(now + 2*tick), delay = 2*tick).runToFuture
        val b = sync.whenAvailable(eventId - 1, until = Some(now + 100*tick), delay = 2*tick).runToFuture
        assert(a ne b)

        scheduler.tick(tick)
        assert(!a.isCompleted)

        scheduler.tick(tick)      // `until` elapsed
        assert(a.isCompleted)
        assert(!a.successValue)   // false: Timed out
        assert(!b.isCompleted)

        sync.onAdded(eventId)
        scheduler.tick()
        assert(!b.isCompleted)    // Still delayed

        scheduler.tick(tick)
        assert(!b.isCompleted)    // Still delayed

        scheduler.tick(tick)
        assert(b.isCompleted)
        assert(b.isCompleted)
        assert(b.successValue)    // true: Event arrived

        assert(sync.waitingCount == 0)
      }
    }
  }

  if (sys.props.contains("test.speed")) "speed" in {
    import Scheduler.Implicits.global

    val sync = new EventSync(initial = EventId.BeforeFirst, EventId.toString)
    val n = 10000
    val eventIdGenerator = new EventIdGenerator
    for (_ <- 1 to 10) {
      val stopwatch = new Stopwatch
      val eventIds = for (_ <- 1 to n) yield eventIdGenerator.next()
      val futures: Seq[Future[Boolean]] = for (eventId <- eventIds) yield
        Task.defer(sync.whenAvailable(after = eventId - 1, until = Some(now + 99.seconds)))
          .runToFuture
      eventIds foreach sync.onAdded
      val result = futures await 99.s
      assert(result forall identity)
      scribe.info(stopwatch.itemsPerSecondString(n, "events"))
      assert(sync.waitingCount == 0)
    }
  }
}
