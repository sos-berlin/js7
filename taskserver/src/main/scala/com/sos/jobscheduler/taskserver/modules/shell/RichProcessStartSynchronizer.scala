package com.sos.scheduler.engine.taskserver.modules.shell

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.taskserver.task.process.RichProcess
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
@ImplementedBy(classOf[StandardRichProcessStartSynchronizer])
trait RichProcessStartSynchronizer extends AutoCloseable with ((⇒ RichProcess) ⇒ Future[RichProcess])

object RichProcessStartSynchronizer {
  val ForTest: RichProcessStartSynchronizer =
    new RichProcessStartSynchronizer {
      def close() = {}
      def apply(o: ⇒ RichProcess) = Future { o } (ExecutionContext.global)
    }
}
