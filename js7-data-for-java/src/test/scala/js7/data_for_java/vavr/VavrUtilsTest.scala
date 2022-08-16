package js7.data_for_java.vavr

import io.vavr.control.Either as VEither
import js7.base.problem.{Problem, ProblemException}
import js7.base.test.Test
import js7.data_for_java.vavr.VavrUtils.getOrThrow

/**
  * @author Joacim Zschimmer
  */
final class VavrUtilsTest extends Test
{
  "getOrThrow" - {
    "Left" in {
      val e = intercept[RuntimeException] {
        getOrThrow(VEither.left(Problem("PROBLEM")))
      }
      assert(e.toString == "java.lang.RuntimeException: PROBLEM")
      assert(e.getMessage == "PROBLEM")
      assert(e.getCause.asInstanceOf[ProblemException].problem == Problem("PROBLEM"))
      assert(e.getCause.asInstanceOf[ProblemException].getMessage == "PROBLEM")
    }

    "Right" in {
      assert(getOrThrow(VEither.right[Problem, Int](7)) == 7)
    }
  }
}
