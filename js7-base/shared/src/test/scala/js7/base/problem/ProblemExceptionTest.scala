package js7.base.problem

import js7.base.test.OurTestSuite

final class ProblemExceptionTest extends OurTestSuite
{
  "NoStackStrace" in {
    assert(Problem("PROBLEM").throwable.isInstanceOf[ProblemException.NoStackTrace])
    assert(Problem("PROBLEM").throwable.getStackTrace.isEmpty)
    assert(Problem("PROBLEM").throwable.toString == "ProblemException: PROBLEM")
  }
}
