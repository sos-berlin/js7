package js7.base.log

import cats.syntax.parallel._
import js7.base.log.CorrelIdBinder.{bindCorrelId, currentCorrelId}
import js7.base.log.CorrelIdBinderTest._
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import monix.eval.{Task, TaskLocal}
import monix.execution.schedulers.{ExecutorScheduler, TracingScheduler}
import monix.execution.{CancelableFuture, ExecutionModel}
import org.apache.logging.log4j.ThreadContext
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now

final class CorrelIdBinderTest extends AnyFreeSpec with BeforeAndAfterAll
{
  Log4j.initialize()

  private val taskBatchSize = 3

  private lazy val underlyingScheduler =
    ExecutorScheduler.forkJoinDynamic("CorrelIdBinderTest",
      parallelism = sys.runtime.availableProcessors(),
      maxThreads = sys.runtime.availableProcessors(),
      daemonic = true,
      reporter = t => println("CorrelIdBinderTest: " + t.toStringWithCauses),
      ExecutionModel.BatchedExecution(taskBatchSize))

  private implicit lazy val scheduler = TracingScheduler(
    underlyingScheduler)

  override def afterAll() = {
    super.afterAll()
    underlyingScheduler.shutdown()
    CorrelIdBinder.logStatistics()
    CorrelIdLog4JThreadContextMap.logStatistics()
  }

  "Manual tests" - {
    // Look at build.log (with debug enabled and %X{js7.correlId} in the pattern)!
    // %X{js7.correlId} should show the same value as $currentCorrelId in the same line.

    "raw" in {
      logger.info(s"${Thread.currentThread.getId} $currentCorrelId First line")
    }

    "Synchronous (Unit)" in {
      val correlId = CorrelId("AAAAAAAA")
      bindCorrelId(correlId) {
        logger.info(s"${Thread.currentThread.getId} $currentCorrelId Synchronous (Unit)")
      }
      logger.info(s"${Thread.currentThread.getId} $currentCorrelId Synchronous (Unit) — not bound")
    }

    "Task.runToFuture" in {
      val correlId = CorrelId("BBBBBBBB")
      val task = Task {
        logger.info(s"${Thread.currentThread.getId} $currentCorrelId Task.runToFuture")
        assert(currentCorrelId == correlId)
      }
      val future = bindCorrelId(correlId) {
        task.runToFuture
      }
      future.await(99.s)
      logger.info(s"${Thread.currentThread.getId} $currentCorrelId Task.runToFuture — not bound")
    }

    "Task.runToFuture with Task.parSequence" in {
      val correlId = CorrelId("CCCCCCCC")
      val task = Task.parSequence(
        for (i <- 1 to 8) yield
          Task {
            sleep(10.ms)
            logger.info(
              s"${Thread.currentThread.getId} $currentCorrelId Task.runToFuture parSequence $i a")
            assert(currentCorrelId == correlId)
          } *>
          Task {
            logger.info(
              s"${Thread.currentThread.getId} $currentCorrelId Task.runToFuture parSequence $i b")
            assert(currentCorrelId == correlId)
          }
      )
      val future = bindCorrelId(correlId) {
        task.runToFuture
      }
      future.await(99.s)
      logger.info(
        s"${Thread.currentThread.getId} $currentCorrelId Task.runToFuture parSequence — not bound")
    }

    "Future" in {
      val correlId = CorrelId("DDDDDDDD")
      val future = bindCorrelId(correlId)(
        Future {
          logger.info(s"${Thread.currentThread.getId} $currentCorrelId Future 1")
          assert(currentCorrelId == correlId)
        }.flatMap(_ => Future {
          logger.info(s"${Thread.currentThread.getId} $currentCorrelId Future 2")
          assert(currentCorrelId == correlId)
        }))
      future.await(99.s)
      logger.info(s"${Thread.currentThread.getId} $currentCorrelId Future — not bound")
    }

    "Task" in {
      val correlId = CorrelId("EEEEEEEE")
      val future: CancelableFuture[Unit] =
        bindCorrelId(CorrelId("_WRONG__")) {
          val task: Task[Unit] =
            bindCorrelId(correlId)(Task {
              logger.info(s"${Thread.currentThread.getId} $currentCorrelId Task")
              assert(currentCorrelId == correlId)
              ()
            })
          task.runToFuture
        }
      future.await(99.s)
      logger.info(s"${Thread.currentThread.getId} $currentCorrelId Task — not bound")
    }
  }

  "bindCorrelId[Task[r]]" in {
    val correlIds = Vector.tabulate(1000)(i => CorrelId(i.toString))
    val n = 200
    for (i <- 1 to n) {
      val t = now
      correlIds
        .parTraverse(correlId =>
          bindCorrelId(correlId)(
            Task.traverse((1 to taskBatchSize + 1))(j => Task {
              //  logger.debug(s"bindCorrelId[Task[r]] $i $correlId $j")
              assert(currentCorrelId == correlId)
              ()
            })(Vector)))
        .await(99.s)
      if (i % (n / 10) == 0) {
        logger.info(itemsPerSecondString(t.elapsed, correlIds.size, "Tasks"))
      }
    }
  }

  "bindCorrelId[Future[r]]" in {
    val correlIds = Vector.tabulate(1000)(i => CorrelId(i.toString))
    val n = 200
    for (i <- 1 to n) {
      val t = now
      Future
        .sequence(correlIds
          .map(correlId =>
            bindCorrelId(correlId)(
              Future.traverse((1 to taskBatchSize + 1).toVector)(j => Future {
                //  logger.debug(s"bindCorrelId[Task[r]] $i $correlId $j")
                assert(currentCorrelId == correlId)
                ()
              }))))
        .await(99.s)
      if (i % (n / 10) == 0) {
        logger.info(itemsPerSecondString(t.elapsed, correlIds.size, "Futures"))
      }
    }
  }

  "bindCorrelId[Unit]" in {
    var a: CorrelId = null
    bindCorrelId(CorrelId("__SYNC__")) {
      a = currentCorrelId
    }
    assert(currentCorrelId.isEmpty)
    assert(a == CorrelId("__SYNC__"))
  }

  "bindCorrelId[String]" in {
    import CanBindCorrelId.implicits.synchronousAsDefault
    val result = bindCorrelId(CorrelId("__SYNC__")) {
      currentCorrelId.string
    }
    assert(currentCorrelId.isEmpty)
    assert(result == "__SYNC__")
  }

  if (false) "ThreadContext implemented with Monix Local, is stiched to the Fiber" in {
    // We do not use ThreadContext.put. Maybe it should work in non-Monix threads, too.
    val task = for {
      _ <- Task(ThreadContext.put("key", "0"))
      _ <- Task(assert(ThreadContext.get("key") == "0"))
      _ <- Task.shift
      _ <- Task(assert(ThreadContext.get("key") == "0"))
      _ <- TaskLocal.isolate(Task(ThreadContext.put("key", "1")))
      _ <- Task(assert(ThreadContext.get("key") == "0"))
    } yield ()
    task.await(99.s)
  }
}

object CorrelIdBinderTest {
  private val logger = Logger[this.type]
}
