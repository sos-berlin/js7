package com.sos.scheduler.engine.base.utils


import com.sos.scheduler.engine.base.utils.StackTraces._
import com.sos.scheduler.engine.base.utils.StackTracesTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class StackTracesTest extends FreeSpec {

  "withThisSackTrace extends possible failures stack strace with own stack trace" in {
    val t = Try[Unit] { throw new TestException }
    assert(!stackTraceContainsCreationsStackTrace { t.get })
    assert(stackTraceContainsCreationsStackTrace { new MyTest().f(t).get })
  }

  private def stackTraceContainsCreationsStackTrace(body: ⇒ Unit): Boolean =
    intercept[TestException] { body } .getStackTrace exists { _.toString contains classOf[MyTest].getName }

  private class TestException extends Exception
}

private object StackTracesTest {
  private class MyTest {
    def f[A](t: Try[A]) = t.appendCurrentStackTrace
  }
}
