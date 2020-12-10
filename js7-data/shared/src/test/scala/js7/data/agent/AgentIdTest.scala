package js7.data.agent

import js7.base.problem.ProblemException
import org.scalatest.freespec.AnyFreeSpec

final class AgentIdTest extends AnyFreeSpec
{
  "AgentId" in {
    // See NameValidating
    assert(AgentId.checked("").isLeft)
    intercept[ProblemException] { AgentId("") }
    assert(AgentId.checked("-").isLeft)
    assert(AgentId.checked("a").isRight)
    assert(AgentId.checked("a-b").isRight)
    assert(AgentId.checked("a.b").isRight)
    assert(AgentId.checked("a_b").isRight)
  }
}
