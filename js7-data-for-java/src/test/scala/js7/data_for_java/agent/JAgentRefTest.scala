package  js7.data_for_java.agent

import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef}
import org.scalatest.freespec.AnyFreeSpec

final class JAgentRefTest extends AnyFreeSpec
{
  "Java test" in {
    new JAgentRefTester(
      JAgentRef(AgentRef(AgentPath("AGENT"), Uri("https://agent.example.com")))
    ).test()
  }
}
