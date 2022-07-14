package js7.base.thread

import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.IOExecutor.ioFuture
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Await
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class IOExecutorTest extends AnyFreeSpec
{
  "Success" in {
    assert(ioFuture(7).await(10.seconds) == 7)
  }

  "Failure" in {
    assert(Await.ready(ioFuture { sys.error("FAILED") }, 10.seconds).value.get.failed.get.toString ==
      "java.lang.RuntimeException: FAILED")
  }

  "Thread name" in {
    ioFuture {
      assert(Thread.currentThread.getName startsWith "JS7 global I/O ")
    } await 10.seconds
  }
}
