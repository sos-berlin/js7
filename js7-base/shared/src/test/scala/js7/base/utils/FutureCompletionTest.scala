package js7.base.utils

import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
final class FutureCompletionTest extends AsyncFreeSpec
{
  "FutureCompletion" in {
    val promise = Promise[Int]()
    val m = new FutureCompletion[Int](promise.future)
    val a = m.add()
    val b = m.add()
    val c = m.add()
    assert(m.size == 3)

    b.close()
    assert(m.size == 2)

    b.close()
    assert(m.size == 2)

    promise.success(7)
    for {
      a1 <- a.future
      c1 <- c.future
    } yield {
      assert(a1 == 7 && !b.future.isCompleted && c1 == 7)
      assert(m.size == 0)
    }
  }
}
