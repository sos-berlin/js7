package js7.base.thread

import js7.base.thread.Futures.implicits._
import js7.base.thread.IOExecutor.ioFuture
import js7.base.time.ScalaTime._
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class IOExecutorTest extends AnyFreeSpec
{
  private implicit val iox = new IOExecutor(name = "IOExecutorTest", 1.s)

  "Success" in {
    assert(ioFuture(7).await(10.seconds) == 7)
  }

  "Failure" in {
    assert(Await.ready(ioFuture { sys.error("FAILED") }, 10.seconds).value.get.failed.get.toString ==
      "java.lang.RuntimeException: FAILED")
  }

  "Thread name" in {
    ioFuture {
      assert(Thread.currentThread.getName startsWith "IOExecutorTest ")
    } await 10.seconds
  }
}
