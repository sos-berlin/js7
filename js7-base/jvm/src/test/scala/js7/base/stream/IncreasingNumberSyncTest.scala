package js7.base.stream

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.implicits.catsSyntaxFlatMapIdOps
import js7.base.catsutils.CatsDeadline
import js7.base.log.Logger
import js7.base.stream.IncreasingNumberSyncTest.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class IncreasingNumberSyncTest extends OurTestSuite:

  private given IORuntime = ioRuntime
  private given ExecutionContext = ioRuntime.compute

  "test" in:
    val sync = new IncreasingNumberSync(initial = 0L, _.toString)
    var waitingCount = 0
    for (aEventId, bEventId, cEventId) <- List((1L, 2L, 3L), (4L, 5L, 6L), (7L, 8L, 9L)) do
      val a = sync.whenAvailable(aEventId, until = None).unsafeToFuture()
      val b = sync.whenAvailable(bEventId, until = None).unsafeToFuture()
      val c = sync.whenAvailable(cEventId, until = None).unsafeToFuture()

      sync.onAdded(aEventId)
      //scheduler.tick()
      assert(!a.isCompleted)
      assert(!b.isCompleted)

      sync.onAdded(bEventId)
      awaitAndAssert(a.isCompleted)
      assert(a.successValue)
      assert(!b.isCompleted)

      sync.onAdded(cEventId)
      assert(!c.isCompleted)

      val a1 = sync.whenAvailable(aEventId, until = None).unsafeToFuture()
      val b1 = sync.whenAvailable(bEventId, until = None).unsafeToFuture()
      val c1 = sync.whenAvailable(cEventId, until = None).unsafeToFuture()
      awaitAndAssert(a1.isCompleted)
      awaitAndAssert(b1.isCompleted)
      assert(!c1.isCompleted)

      // The last whenAvailable has not yet completed (in each test loop iteration)
      assert(sync.waitingCount == 1)
      waitingCount = sync.waitingCount

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
            now <- CatsDeadline.now
            o <- sync.whenAvailable(after = eventId - 1, until = Some(now + 99.seconds))
          yield o)
            .unsafeToFuture()
      eventIds foreach sync.onAdded
      val result = futures.await(99.s)
      assert(result forall identity)
      logger.info(stopwatch.itemsPerSecondString(n, "events"))
      assert(sync.waitingCount == 0)

  if sys.runtime.maxMemory <= 20*1024*1024 then
    "No OutOfMemoryError" in:
      // In practice, OutOfMemoryError does not occur, because clients will not cancel
      // whenAvailable often (as in v2.5).
      // However, IncreasingNumberSync should avoid Promise and return an Cats Effect IO
      val n = 10_000_000
      val since = Deadline.now
      val initial = 0L
      val sync = IncreasingNumberSync(initial = 0L, _.toString)
      IO(false).start.flatMap: initialFiber =>
        (initialFiber, 0).tailRecM: (lastFiber, i) =>
          IO.defer:
            logger.trace(s"—————————————————————————— $i")
            lastFiber.cancel *>
              sync.whenAvailable(after = initial + 1, until = None).start.map: fiber =>
                if i < n then Left((fiber, i + 1)) else Right(fiber)
      .flatMap(_.cancel)
      .unsafeRunSync()
      assert(sync.waitingCount == 1)
      logger.info(itemsPerSecondString(since.elapsed, n, "whenAvailable"))


object IncreasingNumberSyncTest:
  private val logger = Logger[this.type]
