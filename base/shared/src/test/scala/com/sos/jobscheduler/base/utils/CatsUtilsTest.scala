package com.sos.jobscheduler.base.utils

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.base.utils.CatsUtils._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CatsUtilsTest extends FreeSpec
{
  "Validated[Throwable, _] orThrow" in {
    assert((Valid(1): Validated[Throwable, Int]).orThrow == 1)

    val invalid: Validated[Throwable, Int] = Invalid(new IllegalArgumentException("THROWABLE"))
    assert(intercept[IllegalArgumentException](invalid.orThrow).getMessage == "THROWABLE")
  }

  "Validated[Problem, _] orThrow" in {
    assert((Valid(1): Validated[Problem, Int]).orThrow == 1)

    val invalid: Validated[Problem, Int] = Invalid(Problem("PROBLEM"))
    assert(intercept[ProblemException](invalid.orThrow).getMessage == "PROBLEM")
  }
}
