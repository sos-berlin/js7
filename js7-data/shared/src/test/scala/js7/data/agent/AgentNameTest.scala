package js7.data.agent

import js7.base.problem.ProblemException
import org.scalatest.freespec.AnyFreeSpec

final class AgentNameTest extends AnyFreeSpec
{
  "AgentName" in {
    // See NameValidating
    assert(AgentName.checked("").isLeft)
    intercept[ProblemException] { AgentName("") }
    assert(AgentName.checked("-").isLeft)
    assert(AgentName.checked("a").isRight)
    assert(AgentName.checked("a-b").isRight)
    assert(AgentName.checked("a.b").isRight)
    assert(AgentName.checked("a_b").isRight)
  }
}
