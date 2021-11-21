package js7.data.agent

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.item.ItemRevision
import js7.data.subagent.SubagentId
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentRefTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(AgentRef(
      AgentPath("AGENT"),
      directors = Seq(SubagentId("SUBAGENT-1")),
      itemRevision = Some(ItemRevision(7))),
      json"""{
        "path": "AGENT",
        "directors": [ "SUBAGENT-1" ] ,
        "itemRevision": 7
      }""")

    // Compatible with v2.1
    testJson(AgentRef(
      AgentPath("AGENT"),
      directors = Nil,
      uri = Some(Uri("http://127.0.0.1")),
      itemRevision = Some(ItemRevision(7))),
      json"""{
        "path": "AGENT",
        "directors": [],
        "uri": "http://127.0.0.1",
        "itemRevision": 7
      }""")

    // For compatibilty with v2.1
    testJsonDecoder(
      AgentRef(AgentPath("AGENT"),
        directors = Nil,
        uri = Some(Uri("http://127.0.0.1"))),
      json"""{
        "path": "AGENT",
        "uri": "http://127.0.0.1"
      }""")
  }
}
