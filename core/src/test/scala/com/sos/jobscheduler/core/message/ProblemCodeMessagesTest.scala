package com.sos.jobscheduler.core.message

import com.sos.jobscheduler.base.problem.ProblemCode
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProblemCodeMessagesTest extends FreeSpec
{
  "Test" in {
    assert(ProblemCodeMessages.problemCodeToString(ProblemCodes.ForTesting) == Some("Test '$1'"))
    assert(ProblemCodeMessages.problemCodeToString(ProblemCode("MISSING-CODE")) == None)
  }
}
