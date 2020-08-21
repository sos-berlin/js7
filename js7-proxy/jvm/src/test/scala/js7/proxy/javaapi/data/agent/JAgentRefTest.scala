package js7.proxy.javaapi.data.agent

import js7.base.web.Uri
import js7.data.agent.{AgentRef, AgentRefPath}
import org.scalatest.freespec.AnyFreeSpec

final class JAgentRefTest extends AnyFreeSpec
{
  "Java test" in {
    new JAgentRefTester(
      JAgentRef(AgentRef(AgentRefPath("/AGENTREF") ~ "1.0", Uri("https://agent.example.com")))
    ).test()
  }
}
