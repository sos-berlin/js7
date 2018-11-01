package com.sos.jobscheduler.common.monixutils

import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import java.lang.Thread.sleep
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import org.scalatest.FreeSpec
import scala.concurrent.Future
import scala.concurrent.duration._

/** Runs one task after the other.
  * @author Joacim Zschimmer
  */
final class TaskSynchronizerTest extends FreeSpec
{
  "run" in {
    val counter = AtomicInt(0)
    def newTask(i: Int) = Task {
      val start = counter.getAndIncrement()
      sleep(10)
      val end = counter.getAndIncrement()
      (i, start, end)
    }
    val n = 20
    val taskSynchronizer = new TaskSynchronizer[(Int, Int, Int)]
    val futures = 0 until n map newTask map taskSynchronizer.run
    val result = Future.sequence(futures) await 99.seconds
    assert(result == (0 until n).map(i ⇒ (i, 2 * i, 2 * i + 1)))  // Task run sequentially, one after the other
    assert(counter.get == 2 * n)

    // A late Task
    assert(taskSynchronizer.run(newTask(n)).await(99.seconds) == (n, 2 * n, 2 * n + 1))
  }
}
