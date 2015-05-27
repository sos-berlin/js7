package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.Tries.ModifiedStackTraceTry
import com.sos.scheduler.engine.common.scalautil.TriesTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TriesTest extends FreeSpec {

  "withThisSackTrace extends possible failures stack strace with own stack trace" in {
    val t = Try[Unit] { throw new TestException }
    assert(!stackTraceContainsCreationsStackTrace { t.get })
    assert(stackTraceContainsCreationsStackTrace { new MyTest().f(t).get })
  }

  private def stackTraceContainsCreationsStackTrace(body: ⇒ Unit): Boolean =
    intercept[TestException] { body } .getStackTrace exists { _.toString contains classOf[MyTest].getName }

  private class TestException extends Exception
}

private object TriesTest {
  private class MyTest {
    def f[A](t: Try[A]) = t.withThisStackTrace
  }
}
