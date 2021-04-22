package js7.data.agent

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.item.ItemRevision
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentRefTest extends AnyFreeSpec
{
  "JSON" in {
    testJsonDecoder(AgentRef(AgentPath("AGENT"), Uri("http://127.0.0.1")),
      json"""{
        "path": "AGENT",
        "uri": "http://127.0.0.1"
      }""")

    testJson(AgentRef(AgentPath("AGENT"), Uri("http://127.0.0.1"), Some(ItemRevision(7))),
      json"""{
        "path": "AGENT",
        "uri": "http://127.0.0.1",
        "itemRevision": 7
      }""")
  }
}
