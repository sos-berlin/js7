package js7.base.log

import cats.syntax.parallel.*
import java.lang.Thread.currentThread
import js7.base.log.CorrelId.current
import js7.base.log.CorrelIdJvmTest.*
import js7.base.system.Java8Polyfill.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import monix.eval.{Task, TaskLocal}
import monix.execution.schedulers.{ExecutorScheduler, TracingScheduler}
import monix.execution.{CancelableFuture, ExecutionModel}
import org.apache.logging.log4j.ThreadContext
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now

final class CorrelIdJvmTest extends OurTestSuite, BeforeAndAfterAll
{
  private val taskBatchSize = 3

  private lazy val underlyingScheduler =
    ExecutorScheduler.forkJoinDynamic("CorrelIdJvmTest",
      parallelism = sys.runtime.availableProcessors(),
      maxThreads = sys.runtime.availableProcessors(),
      daemonic = true,
      reporter = t => println("CorrelIdJvmTest: " + t.toStringWithCauses),
      ExecutionModel.BatchedExecution(taskBatchSize))

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

    "Task.runToFuture" in {
      val correlId = CorrelId("BBBBBBBB")
      val task = Task {
        logger.info(s"${currentThread.threadId} $current Task.runToFuture")
        assert(current == correlId)
      }
      val future = correlId.bind {
        task.runToFuture
      }
      future.await(99.s)
      logger.info(s"${currentThread.threadId} $current Task.runToFuture — not bound")
    }

    "Task.runToFuture with Task.parSequence" in {
      val correlId = CorrelId("CCCCCCCC")
      val task = Task.parSequence(
        for i <- 1 to 8 yield
          Task {
            sleep(10.ms)
            logger.info(
              s"${currentThread.threadId} $current Task.runToFuture parSequence $i a")
            assert(current == correlId)
          } *>
          Task {
            logger.info(
              s"${currentThread.threadId} $current Task.runToFuture parSequence $i b")
            assert(current == correlId)
          }
      )
      val future = correlId.bind {
        task.runToFuture
      }
      future.await(99.s)
      logger.info(
        s"${currentThread.threadId} $current Task.runToFuture parSequence — not bound")
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

    "Task" in {
      val correlId = CorrelId("EEEEEEEE")
      val future: CancelableFuture[Unit] =
        CorrelId("_WRONG__").bind {
          val task: Task[Unit] =
            correlId.bind(Task {
              logger.info(s"${currentThread.threadId} $current Task")
              assert(current == correlId)
              ()
            })
          task.runToFuture
        }
      future.await(99.s)
      logger.info(s"${currentThread.threadId} $current Task — not bound")
    }
  }

  "bindCorrelId[Task[r]]" in {
    val correlIds = Vector.tabulate(1000)(i => CorrelId(i.toString))
    val n = 200
    for i <- 1 to n do {
      val t = now
      correlIds
        .parTraverse(correlId =>
          correlId.bind(
            Task.traverse((1 to taskBatchSize + 1))(j => Task {
              //  logger.debug(s"bindCorrelId[Task[r]] $i $correlId $j")
              assert(current == correlId)
              ()
            })(Vector)))
        .await(99.s)
      if i % (n / 10) == 0 then {
        logger.info(itemsPerSecondString(t.elapsed, correlIds.size, "Tasks"))
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
              Future.traverse((1 to taskBatchSize + 1).toVector)(j => Future {
                //  logger.debug(s"bindCorrelId[Task[r]] $i $correlId $j")
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
    val task = for
      _ <- Task(ThreadContext.put("key", "0"))
      _ <- Task(assert(ThreadContext.get("key") == "0"))
      _ <- Task.shift
      _ <- Task(assert(ThreadContext.get("key") == "0"))
      _ <- TaskLocal.isolate(Task(ThreadContext.put("key", "1")))
      _ <- Task(assert(ThreadContext.get("key") == "0"))
    yield ()
    task.await(99.s)
  }
}


object CorrelIdJvmTest {
  private val logger = Logger[this.type]
  java8Polyfill()

}
