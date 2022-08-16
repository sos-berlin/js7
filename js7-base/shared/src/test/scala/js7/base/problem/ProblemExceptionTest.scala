package js7.base.problem

import js7.base.test.Test

final class ProblemExceptionTest extends Test
{
  "NoStackStrace" in {
    assert(Problem("PROBLEM").throwable.isInstanceOf[ProblemException.NoStackTrace])
    assert(Problem("PROBLEM").throwable.getStackTrace.isEmpty)
    assert(Problem("PROBLEM").throwable.toString == "ProblemException: PROBLEM")
  }
}
