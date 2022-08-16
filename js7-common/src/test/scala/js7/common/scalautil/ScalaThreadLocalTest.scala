package js7.common.scalautil

import js7.base.test.Test
import js7.common.scalautil.ScalaThreadLocal.threadLocal
import org.scalatest.concurrent.Waiters.Waiter

/**
 * @author Joacim Zschimmer
 */
final class ScalaThreadLocalTest extends Test {

  "ScalaThreadLocal" in {
    @volatile var count = 0
    val t = threadLocal {
      count += 1
      count
    }
    assertResult(1)(t())
    assertResult(1)(t())
    assertResult(1)(t: Int)  // Implicit convertion
    val w = new Waiter
    val thread = new Thread {
      override def run() = {
        w {
          assertResult(2)(t())
          assertResult(2)(t())
        }
        w.dismiss()
      }
    }
    thread.start()
    thread.join()
    w.await()
  }
}
