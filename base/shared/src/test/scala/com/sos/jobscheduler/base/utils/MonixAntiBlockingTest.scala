package com.sos.jobscheduler.base.utils

import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class MonixAntiBlockingTest extends FreeSpec
{
  if (sys.props.get("scala.concurrent.context.numThreads") contains "1") {
    "Trying to minimize blocking error ..." in {
      implicit val scheduler = Scheduler.global
      val task = Task.deferFuture {
        Future {
          val p = Promise[Int]()
          Future { scheduler execute { () ⇒ p.success(7) } }
          Await.result(p.future, 1.second)
        }
      }
      assert(Await.result(task.runToFuture, 1.second) == 7)
    }
  }
}

