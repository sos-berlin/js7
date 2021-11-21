package js7.data_for_java.agent

import js7.data.agent.{AgentPath, AgentRef}
import js7.data.subagent.SubagentId
import org.scalatest.freespec.AnyFreeSpec

final class JAgentRefTest extends AnyFreeSpec
{
  "Java test" in {
    new JAgentRefTester(
      JAgentRef(AgentRef(AgentPath("AGENT"), directors = Seq(SubagentId("SUBAGENT"))))
    ).test()
  }
}
