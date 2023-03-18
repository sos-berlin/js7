package js7.base.thread

import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import java.util.concurrent.Executor
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.IOExecutor.ioFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.thread.ThreadPoolsBase.newBlockingNonVirtualThreadPool
import js7.base.thread.VirtualThreads.maybeNewVirtualThreadExecutorService
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class IOExecutorTest extends OurTestSuite
{
  private val logger = Logger[this.type]

  "Success" in {
    assert(ioFuture(7).await(10.seconds) == 7)
  }

  "Failure" in {
    assert(Await.ready(ioFuture { sys.error("FAILED") }, 10.seconds).value.get.failed.get.toString ==
      "java.lang.RuntimeException: FAILED")
  }

  if (!VirtualThreads.isEnabled) {
    "Thread name" in {
      ioFuture {
        assert(Thread.currentThread.getName startsWith "JS7 global I/O-")
      } await 10.seconds
    }
  }

  if (sys.props.contains("test.speed")) {
    if (VirtualThreads.isEnabled) {
      "Performance with VirtualThread" in {
        for (executor <- maybeNewVirtualThreadExecutorService()) {
          testPerformance(executor, 1000)
          testPerformance(executor, 10000)
          testPerformance(executor, 100000)
          testPerformance(executor, 200000)
          testPerformance(executor, 1000000) // May require big heap
          executor.shutdown()
        }
      }
    }

    "Performance with thread pool" in {
      val executor = newBlockingNonVirtualThreadPool("IOExecutorTest")
      testPerformance(executor, 1000)
      testPerformance(executor, 10000)
      testPerformance(executor, 100000)
      executor.shutdown()
    }
  }

  private def testPerformance(executor: Executor, n: Int): Unit = {
    val iox = new IOExecutor(executor, "IOExecutorTest")
    val since = now
    val task = (1 to n).toVector
      .traverse(_ => iox(Task { sleep(100.ms) }).start)
      .map(_.map(_.join))
      .map(_.combineAll)
      .flatten
    for (i <- 1 to 3) {
      task.await(99.s)
      logger.info(itemsPerSecondString(since.elapsed, n))
    }
  }
}
