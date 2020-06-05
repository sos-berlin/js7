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
  "JSON, anonymous" in {
    testJson(AgentRef(AgentRefPath.NoId, Uri("http://127.0.0.1")),
      json"""{
        "uri": "http://127.0.0.1"
      }""")
  }

  "JSON, with ID" in {
    testJson(AgentRef(AgentRefPath("/AGENT") ~ "VERSION", Uri("http://127.0.0.1")),
      json"""{
        "path": "/AGENT",
        "versionId": "VERSION",
        "uri": "http://127.0.0.1"
      }""")
  }
}
