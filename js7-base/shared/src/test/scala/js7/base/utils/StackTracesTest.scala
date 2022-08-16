package js7.base.utils

import js7.base.test.Test
import js7.base.utils.StackTraces.*
import js7.base.utils.StackTracesTest.*
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
final class StackTracesTest extends Test {

  "withThisSackTrace extends possible failures stack strace with own stack trace" in {
    val t = Try[Unit] { throw new TestException }
    assert(!stackTraceContainsCreationsStackTrace { t.get })
    assert(stackTraceContainsCreationsStackTrace { new MyTest().f(t).get })
  }

  private def stackTraceContainsCreationsStackTrace(body: => Unit): Boolean =
    intercept[TestException] { body } .getStackTrace.exists(_.toString contains classOf[MyTest].getName)

  private class TestException extends Exception
}

private object StackTracesTest {
  private class MyTest {
    def f[A](t: Try[A]) = t.appendCurrentStackTrace
  }
}
