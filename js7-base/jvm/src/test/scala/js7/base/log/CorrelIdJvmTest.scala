package js7.base.log

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.instances.vector.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import java.lang.Thread.currentThread
import js7.base.log.CorrelId.current
import js7.base.log.CorrelIdJvmTest.*
import js7.base.log.log4j.Log4jThreadContextMap
import js7.base.system.Java17Polyfill.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.duration.Deadline.now
import scala.concurrent.{ExecutionContext, Future}

final class CorrelIdJvmTest extends OurTestSuite, BeforeAndAfterAll:

  private given IORuntime = ioRuntime
  private given ExecutionContext = ioRuntime.compute

  private val ioBatchSize = 3

  //private lazy val underlyingScheduler =
  //  ExecutorScheduler.forkJoinDynamic("CorrelIdJvmTest",
  //    parallelism = sys.runtime.availableProcessors(),
  //    maxThreads = sys.runtime.availableProcessors(),
  //    daemonic = true,
  //    reporter = t => println("CorrelIdJvmTest: " + t.toStringWithCauses),
  //    ExecutionModel.BatchedExecution(ioBatchSize))

  override def afterAll() =
    super.afterAll()
    //underlyingScheduler.shutdown()
    CorrelId.logStatistics()
    Log4jThreadContextMap.logStatistics()

  "Manual tests" - {
    // Look at build.log (with debug enabled and %X{js7.correlId} in the pattern)!
    // %X{js7.correlId} should show the same value as $current in the same line.

    "raw" in:
      logger.info(s"${currentThread.threadId} $current First line")

    "Synchronous (Unit)" in:
      val correlId = CorrelId("AAAAAAAA")
      correlId.bind:
        logger.info(s"${currentThread.threadId} $current Synchronous (Unit)")
      logger.info(s"${currentThread.threadId} $current Synchronous (Unit) — not bound")

    "IO.unsafeToFuture()" in:
      pending // TODO
      val correlId = CorrelId("BBBBBBBB")
      val io = IO:
        logger.info(s"${currentThread.threadId} $current IO.unsafeToFuture()")
        assert(current == correlId)
      val future = correlId.bind:
        io.unsafeToFuture()
      future.await(99.s)
      logger.info(s"${currentThread.threadId} $current IO.unsafeToFuture() — not bound")

    "IO.unsafeToFuture() with parTraverse" in:
      pending // TODO
      val correlId = CorrelId("CCCCCCCC")
      val io =
        (1 to 8).toVector.parTraverse: i =>
          IO:
            sleep(10.ms)
            logger.info(
              s"${currentThread.threadId} $current IO.unsafeToFuture() parSequence $i a")
            assert(current == correlId)
          *>
            IO:
              logger.info(
                s"${currentThread.threadId} $current IO.unsafeToFuture() parSequence $i b")
              assert(current == correlId)
      val future = correlId.bind:
        io.unsafeToFuture()
      future.await(99.s)
      logger.info(
        s"${currentThread.threadId} $current IO.unsafeToFuture() parSequence — not bound")

    "Future" in:
      pending // TODO
      val correlId = CorrelId("DDDDDDDD")
      val future = correlId.bind:
        Future:
          logger.info(s"${currentThread.threadId} $current Future 1")
          assert(current == correlId)
        .flatMap: _ =>
          Future:
            logger.info(s"${currentThread.threadId} $current Future 2")
            assert(current == correlId)
      future.await(99.s)
      logger.info(s"${currentThread.threadId} $current Future — not bound")

    "IO" in:
      pending // TODO
      val correlId = CorrelId("EEEEEEEE")
      val future: Future[Unit] =
        CorrelId("_WRONG__").bind:
          val io: IO[Unit] =
            correlId.bind:
              IO:
                logger.info(s"${currentThread.threadId} $current IO")
                assert(current == correlId)
                ()
          io.unsafeToFuture()
      future.await(99.s)
      logger.info(s"${currentThread.threadId} $current IO — not bound")
  }

  "bindCorrelId[IO[r]]" in:
    pending // TODO
    val correlIds = Vector.tabulate(1000)(i => CorrelId(i.toString))
    val n = 200
    for i <- 1 to n do
      val t = now
      correlIds
        .parTraverse: correlId =>
          correlId.bind:
            (1 to ioBatchSize + 1).toVector.traverse: j =>
              IO:
                //  logger.debug(s"bindCorrelId[IO[r]] $i $correlId $j")
                assert(current == correlId)
                ()
        .await(99.s)
      if i % (n / 10) == 0 then
        logger.info(itemsPerSecondString(t.elapsed, correlIds.size, "IOs"))

  "bindCorrelId[Future[r]]" in:
    pending // TODO
    val correlIds = Vector.tabulate(1000)(i => CorrelId(i.toString))
    val n = 200
    for i <- 1 to n do
      val t = now
      Future
        .sequence(correlIds
          .map: correlId =>
            correlId.bind(
              Future.traverse((1 to ioBatchSize + 1).toVector)(j => Future:
                //  logger.debug(s"bindCorrelId[IO[r]] $i $correlId $j")
                assert(current == correlId)
                ()
              )))
        .await(99.s)
      if i % (n / 10) == 0 then
        logger.info(itemsPerSecondString(t.elapsed, correlIds.size, "Futures"))

  //if false then "ThreadContext implemented with Monix Local, is stiched to the Fiber" in:
  //  // We do not use ThreadContext.put. Maybe it should work in non-Monix threads, too.
  //  val io = for
  //    _ <- IO(ThreadContext.put("key", "0"))
  //    _ <- IO(assert(ThreadContext.get("key") == "0"))
  //    _ <- IO.cede
  //    _ <- IO(assert(ThreadContext.get("key") == "0"))
  //    _ <- IOLocal.isolate(IO(ThreadContext.put("key", "1")))
  //    _ <- IO(assert(ThreadContext.get("key") == "0"))
  //  yield ()
  //  io.await(99.s)


object CorrelIdJvmTest:
  private val logger = Logger[this.type]
  java17Polyfill()
