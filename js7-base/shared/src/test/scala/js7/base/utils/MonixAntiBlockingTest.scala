package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Await, Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class MonixAntiBlockingTest extends OurTestSuite:
  if sys.props.get("scala.concurrent.context.numThreads") contains "1" then
    "Trying to minimize blocking error ..." in:
      implicit val scheduler = Scheduler.traced
      val task = Task.deferFuture:
        Future:
          val p = Promise[Int]()
          Future { scheduler execute { () => p.success(7) } }
          Await.result(p.future, 1.s)
      assert(Await.result(task.runToFuture, 1.s) == 7)
