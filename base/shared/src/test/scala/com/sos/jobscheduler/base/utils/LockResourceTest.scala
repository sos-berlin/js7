package com.sos.jobscheduler.base.utils

import cats.effect.Resource
import com.sos.jobscheduler.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.AsyncFreeSpec
/**
  * @author Joacim Zschimmer
  */
final class LockResourceTest extends AsyncFreeSpec
{
  private val n = 1000
  private val initial = 1

  "LockResource" in {
    doTest(LockResource())
      .runToFuture
      .map(o => assert(o == Vector.fill(n)(initial)))
  }

  "Other Resource does not lock" in {
    val nonLockingResource = Resource.make(Task.unit)(_ => Task.unit)
    doTest(nonLockingResource)
      .runToFuture
      .map(o => assert(o != Vector.fill(n)(initial)))
  }

  private def doTest(lockResource: Resource[Task, Unit]): Task[Seq[Int]] = {
    @volatile var variable = initial
    val tasks = (1 to n).map(i => lockResource.use { _ =>
      Task {
        val found = variable
        variable += 1
        found
      }.flatMap { found =>
        Task.sleep(100.µs) >>
        Task {
          variable = initial
          found
        }
      }
    })
    Task.gather(tasks)
  }
}
