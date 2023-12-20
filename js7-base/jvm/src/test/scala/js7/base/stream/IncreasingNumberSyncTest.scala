package js7.base.stream

import js7.base.catsutils.CatsDeadline
import js7.base.log.Logger
import js7.base.stream.IncreasingNumberSyncTest.*
import js7.base.test.{OurTestSuite, TestCatsEffect}
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import scala.concurrent.Future
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class IncreasingNumberSyncTest extends OurTestSuite, TestCatsEffect:

  //FIXME Monix
  //monix "test" in:
  //monix   implicit val scheduler = TestScheduler()
  //monix
  //monix   val sync = new IncreasingNumberSync(initial = 0L, _.toString)
  //monix   var waitingCount = 0
  //monix   for (aEventId, bEventId, cEventId) <- List((1L, 2L, 3L), (4L, 5L, 6L), (7L, 8L, 9L)) do
  //monix     val a = sync.whenAvailable(aEventId, until = None).unsafeToFuture()
  //monix     val b = sync.whenAvailable(bEventId, until = None).unsafeToFuture()
  //monix     val c = sync.whenAvailable(cEventId, until = None).unsafeToFuture()
  //monix
  //monix     sync.onAdded(aEventId)
  //monix     scheduler.tick()
  //monix     assert(!a.isCompleted)
  //monix     assert(!b.isCompleted)
  //monix
  //monix     sync.onAdded(bEventId)
  //monix     scheduler.tick()
  //monix     assert(a.isCompleted)
  //monix     assert(a.successValue)
  //monix     assert(!b.isCompleted)
  //monix
  //monix     sync.onAdded(cEventId)
  //monix     scheduler.tick()
  //monix     assert(!c.isCompleted)
  //monix
  //monix     assert(sync.whenAvailable(aEventId, until = None).unsafeToFuture().isCompleted)
  //monix     assert(sync.whenAvailable(bEventId, until = None).unsafeToFuture().isCompleted)
  //monix     assert(!sync.whenAvailable(cEventId, until = None).unsafeToFuture().isCompleted)
  //monix
  //monix     assert(sync.waitingCount == 1)  // The last whenAvailable has not yet completed (in each test loop iteration)
  //monix     waitingCount = sync.waitingCount
  //monix
  //monix "timeout" in:
  //monix   implicit val scheduler = TestScheduler()
  //monix   val tick = 1.s
  //monix   val sync = new IncreasingNumberSync(initial = 0L, _.toString)
  //monix   for eventId <- 1L to 3L do
  //monix     withClue(s"#$eventId"):
  //monix       val a =
  //monix         (for
  //monix           time <- CatsDeadline.monotonicTime
  //monix           o <- sync.whenAvailable(eventId - 1, until = Some(time + 2*tick), delay = 2*tick)
  //monix         yield o)
  //monix           .unsafeToFuture()
  //monix       val b =
  //monix         (for
  //monix           time <- CatsDeadline.monotonicTime
  //monix           o <- sync.whenAvailable(eventId - 1, until = Some(time + 100*tick), delay = 2*tick)
  //monix         yield o)
  //monix           .unsafeToFuture()
  //monix       assert(a ne b)
  //monix
  //monix       scheduler.tick(tick)
  //monix       assert(!a.isCompleted)
  //monix
  //monix       scheduler.tick(tick)      // `until` elapsed
  //monix       assert(a.isCompleted)
  //monix       assert(!a.successValue)   // false: Timed out
  //monix       assert(!b.isCompleted)
  //monix
  //monix       sync.onAdded(eventId)
  //monix       scheduler.tick()
  //monix       assert(!b.isCompleted)    // Still delayed
  //monix
  //monix       scheduler.tick(tick)
  //monix       assert(!b.isCompleted)    // Still delayed
  //monix
  //monix       scheduler.tick(tick)
  //monix       assert(b.isCompleted)
  //monix       assert(b.isCompleted)
  //monix       assert(b.successValue)    // true: Event arrived
  //monix
  //monix       assert(sync.waitingCount == 0)

  if sys.props.contains("test.speed") then "speed" in:
    val sync = new IncreasingNumberSync(initial = 0L, _.toString)
    val n = 10000
    val eventIdGenerator = Iterator.from(1).map(_.toLong)
    for _ <- 1 to 10 do
      val stopwatch = new Stopwatch
      val eventIds = for _ <- 1 to n yield eventIdGenerator.next()
      val futures: Seq[Future[Boolean]] =
        for eventId <- eventIds yield
          (for
            now <- CatsDeadline.monotonicTime
            o <- sync.whenAvailable(after = eventId - 1, until = Some(now + 99.seconds))
          yield o)
            .unsafeToFuture()
      eventIds foreach sync.onAdded
      val result = futures await 99.s
      assert(result forall identity)
      logger.info(stopwatch.itemsPerSecondString(n, "events"))
      assert(sync.waitingCount == 0)


object IncreasingNumberSyncTest:
  private val logger = Logger[this.type]
