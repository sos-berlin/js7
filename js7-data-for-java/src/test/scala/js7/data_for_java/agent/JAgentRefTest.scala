package js7.data_for_java.agent

import js7.base.test.Test
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.subagent.SubagentId

final class JAgentRefTest extends Test
{
  "Java test" in {
    new JAgentRefTester(
      JAgentRef(AgentRef(AgentPath("AGENT"), directors = Seq(SubagentId("SUBAGENT"))))
    ).test()
  }
}
