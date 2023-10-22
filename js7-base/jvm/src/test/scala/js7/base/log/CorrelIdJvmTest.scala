package js7.base.log

import cats.syntax.parallel.*
import java.lang.Thread.currentThread
import js7.base.log.CorrelId.current
import js7.base.log.CorrelIdJvmTest.*
import js7.base.system.Java8Polyfill.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.CatsBlocking.syntax.RichIO
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import cats.effect.IO
import cats.effect.Fiber
import monix.execution.schedulers.{ExecutorScheduler, TracingScheduler}
import monix.execution.{CancelableFuture, ExecutionModel}
import org.apache.logging.log4j.ThreadContext
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now

final class CorrelIdJvmTest extends OurTestSuite, BeforeAndAfterAll
{
  private val ioBatchSize = 3

  private lazy val underlyingScheduler =
    ExecutorScheduler.forkJoinDynamic("CorrelIdJvmTest",
      parallelism = sys.runtime.availableProcessors(),
      maxThreads = sys.runtime.availableProcessors(),
      daemonic = true,
      reporter = t => println("CorrelIdJvmTest: " + t.toStringWithCauses),
      ExecutionModel.BatchedExecution(ioBatchSize))

  private implicit lazy val scheduler: TracingScheduler =
    TracingScheduler(underlyingScheduler)

  override def afterAll() = {
    super.afterAll()
    underlyingScheduler.shutdown()
    CorrelId.logStatistics()
    CorrelIdLog4jThreadContextMap.logStatistics()
  }

  "Manual tests" - {
    // Look at build.log (with debug enabled and %X{js7.correlId} in the pattern)!
    // %X{js7.correlId} should show the same value as $current in the same line.

    "raw" in {
      logger.info(s"${currentThread.threadId} $current First line")
    }

    "Synchronous (Unit)" in {
      val correlId = CorrelId("AAAAAAAA")
      correlId.bind {
        logger.info(s"${currentThread.threadId} $current Synchronous (Unit)")
      }
      logger.info(s"${currentThread.threadId} $current Synchronous (Unit) — not bound")
    }

    "IO.runToFuture" in {
      val correlId = CorrelId("BBBBBBBB")
      val io = IO {
        logger.info(s"${currentThread.threadId} $current IO.runToFuture")
        assert(current == correlId)
      }
      val future = correlId.bind {
        io.runToFuture
      }
      future.await(99.s)
      logger.info(s"${currentThread.threadId} $current IO.runToFuture — not bound")
    }

    "IO.runToFuture with IO.parSequence" in {
      val correlId = CorrelId("CCCCCCCC")
      val io = IO.parSequence(
        for i <- 1 to 8 yield
          IO {
            sleep(10.ms)
            logger.info(
              s"${currentThread.threadId} $current IO.runToFuture parSequence $i a")
            assert(current == correlId)
          } *>
          IO {
            logger.info(
              s"${currentThread.threadId} $current IO.runToFuture parSequence $i b")
            assert(current == correlId)
          }
      )
      val future = correlId.bind {
        io.runToFuture
      }
      future.await(99.s)
      logger.info(
        s"${currentThread.threadId} $current IO.runToFuture parSequence — not bound")
    }

    "Future" in {
      val correlId = CorrelId("DDDDDDDD")
      val future = correlId.bind(
        Future {
          logger.info(s"${currentThread.threadId} $current Future 1")
          assert(current == correlId)
        }.flatMap(_ => Future {
          logger.info(s"${currentThread.threadId} $current Future 2")
          assert(current == correlId)
        }))
      future.await(99.s)
      logger.info(s"${currentThread.threadId} $current Future — not bound")
    }

    "IO" in {
      val correlId = CorrelId("EEEEEEEE")
      val future: CancelableFuture[Unit] =
        CorrelId("_WRONG__").bind {
          val io: IO[Unit] =
            correlId.bind(IO {
              logger.info(s"${currentThread.threadId} $current IO")
              assert(current == correlId)
              ()
            })
          io.runToFuture
        }
      future.await(99.s)
      logger.info(s"${currentThread.threadId} $current IO — not bound")
    }
  }

  "bindCorrelId[IO[r]]" in {
    val correlIds = Vector.tabulate(1000)(i => CorrelId(i.toString))
    val n = 200
    for i <- 1 to n do {
      val t = now
      correlIds
        .parTraverse(correlId =>
          correlId.bind(
            IO.traverse((1 to ioBatchSize + 1))(j => IO {
              //  logger.debug(s"bindCorrelId[IO[r]] $i $correlId $j")
              assert(current == correlId)
              ()
            })(Vector)))
        .await(99.s)
      if i % (n / 10) == 0 then {
        logger.info(itemsPerSecondString(t.elapsed, correlIds.size, "IOs"))
      }
    }
  }

  "bindCorrelId[Future[r]]" in {
    val correlIds = Vector.tabulate(1000)(i => CorrelId(i.toString))
    val n = 200
    for i <- 1 to n do {
      val t = now
      Future
        .sequence(correlIds
          .map(correlId =>
            correlId.bind(
              Future.traverse((1 to ioBatchSize + 1).toVector)(j => Future {
                //  logger.debug(s"bindCorrelId[IO[r]] $i $correlId $j")
                assert(current == correlId)
                ()
              }))))
        .await(99.s)
      if i % (n / 10) == 0 then {
        logger.info(itemsPerSecondString(t.elapsed, correlIds.size, "Futures"))
      }
    }
  }

  if false then "ThreadContext implemented with Monix Local, is stiched to the Fiber" in {
    // We do not use ThreadContext.put. Maybe it should work in non-Monix threads, too.
    val io = for
      _ <- IO(ThreadContext.put("key", "0"))
      _ <- IO(assert(ThreadContext.get("key") == "0"))
      _ <- IO.shift
      _ <- IO(assert(ThreadContext.get("key") == "0"))
      _ <- IOLocal.isolate(IO(ThreadContext.put("key", "1")))
      _ <- IO(assert(ThreadContext.get("key") == "0"))
    yield ()
    io.await(99.s)
  }
}


object CorrelIdJvmTest {
  private val logger = Logger[this.type]
  java8Polyfill()

}
