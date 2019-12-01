package com.sos.jobscheduler.base.monixutils

import com.sos.jobscheduler.base.monixutils.MonixBase._
import com.sos.jobscheduler.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
final class MonixBaseTest extends FreeSpec
{
  private val sleepyTask = Task(3).delayExecution(200.ms)

  "maybeTimeout Duration.Inf" in {
    assert(sleepyTask.maybeTimeout(Duration.Inf).runSyncUnsafe(99.s) == 3)
  }

  "maybeTimeout FiniteDuration" in {
    intercept[TimeoutException] {
      sleepyTask.maybeTimeout(10.ms).runSyncUnsafe(9999.s)
    }
  }

  "orTimeout" in {
    assert(sleepyTask.orTimeout(10.ms, Task(7)).runSyncUnsafe(99.s) == 7)
  }
}
