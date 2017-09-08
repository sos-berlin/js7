package com.sos.jobscheduler.taskserver.modules.shell

import com.google.inject.ImplementedBy
import com.sos.jobscheduler.common.scalautil.Futures.blockingFuture
import com.sos.jobscheduler.taskserver.task.process.RichProcess
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
@ImplementedBy(classOf[StandardRichProcessStartSynchronizer])
trait RichProcessStartSynchronizer extends AutoCloseable with ((⇒ RichProcess) ⇒ Future[RichProcess])

object RichProcessStartSynchronizer {

  val ForTest: RichProcessStartSynchronizer =
    new RichProcessStartSynchronizer {
      def close() = {}
      def apply(o: ⇒ RichProcess) = blockingFuture { o }
    }
}
