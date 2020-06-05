package js7.taskserver.modules.shell

import com.google.inject.ImplementedBy
import js7.common.scalautil.Futures.blockingThreadFuture
import js7.taskserver.task.process.RichProcess
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
@ImplementedBy(classOf[StandardRichProcessStartSynchronizer])
trait RichProcessStartSynchronizer extends AutoCloseable with ((=> RichProcess) => Future[RichProcess])

object RichProcessStartSynchronizer {

  val ForTest: RichProcessStartSynchronizer =
    new RichProcessStartSynchronizer {
      def close() = {}
      def apply(o: => RichProcess) = blockingThreadFuture { o }
    }
}
