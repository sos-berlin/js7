package js7.data.agent

import js7.base.problem.ProblemException
import js7.base.standards.Js7PathValidatorTest
import js7.base.test.OurTestSuite

final class AgentPathTest extends OurTestSuite:
  "AgentPath" in:
    // See NameValidating
    assert(AgentPath.checked("").isLeft)
    intercept[ProblemException] { AgentPath("") }
    assert(AgentPath.checked("-").isLeft)
    assert(AgentPath.checked("a").isRight)
    assert(AgentPath.checked("a-b").isRight)
    assert(AgentPath.checked("a.b").isRight)
    assert(AgentPath.checked("a_b").isRight)

  "Invalid path" in:
    Js7PathValidatorTest.checkInvalid("AgentPath", AgentPath.checked)

  "Valid paths" in:
    Js7PathValidatorTest.checkValid(AgentPath.checked)
