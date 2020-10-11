package js7.proxy.javaapi.data.agent

import js7.base.web.Uri
import js7.data.agent.{AgentName, AgentRef}
import org.scalatest.freespec.AnyFreeSpec

final class JAgentRefTest extends AnyFreeSpec
{
  "Java test" in {
    new JAgentRefTester(
      JAgentRef(AgentRef(AgentName("AGENT"), Uri("https://agent.example.com")))
    ).test()
  }
}
