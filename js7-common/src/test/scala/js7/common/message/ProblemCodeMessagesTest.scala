package js7.common.message

import js7.base.problem.{ProblemCode, TestCodeProblem}
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class ProblemCodeMessagesTest extends OurTestSuite
{
  ProblemCodeMessages.initialize()

  "problemCodeToPattern is using resource messages-en-conf" in {
    assert(ProblemCodeMessages.problemCodeToPattern(ProblemCode("TestCode")) == Some("TestMessage argument=$argument"))
    assert(ProblemCodeMessages.problemCodeToPattern(ProblemCode("MISSING-CODE")) == None)
  }

  "TestCodeProblem.message" in {
    assert(TestCodeProblem(Map("argument" -> "ARGUMENT")).message == "TestMessage argument=ARGUMENT")
  }

  "TestCodeProblem.toString" in {
    assert(TestCodeProblem(Map("argument" -> "ARGUMENT")).toString == "TestCode: TestMessage argument=ARGUMENT")
  }

  "TestCodeProblem with extra arguments" in {
    assert(TestCodeProblem(Map("argument" -> "ARGUMENT", "extra1" -> "EXTRA1", "extra2" -> "EXTRA2")).toString ==
      "TestCode: TestMessage argument=ARGUMENT (extra1=EXTRA1, extra2=EXTRA2)")
  }

  "TestCodeProblem without referenced arguments" in {
    assert(TestCodeProblem(Map.empty).toString == "TestCode: TestMessage argument=$argument")
  }
}
