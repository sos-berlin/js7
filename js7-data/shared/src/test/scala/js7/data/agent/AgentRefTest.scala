package js7.data.agent

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentRefTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(AgentRef(AgentId("AGENT"), Uri("http://127.0.0.1")),
      json"""{
        "id": "AGENT",
        "uri": "http://127.0.0.1"
      }""")
  }
}
