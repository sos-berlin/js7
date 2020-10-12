package js7.base.problem

import org.scalatest.freespec.AnyFreeSpec

final class ProblemExceptionTest extends AnyFreeSpec
{
  "NoStackStrace" in {
    assert(Problem("PROBLEM").throwable.isInstanceOf[ProblemException.NoStackTrace])
    assert(Problem("PROBLEM").throwable.getStackTrace.isEmpty)
    assert(Problem("PROBLEM").throwable.toString == "ProblemException: PROBLEM")
  }
}
