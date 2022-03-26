package js7.data.subagent

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.utils.Base64UUID
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.delegate.DelegateCouplingState
import js7.data.item.ItemRevision
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SubagentItemStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[SubagentItemState](
      SubagentItemState(
        SubagentItem(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          Uri("https://example.com"),
          itemRevision = Some(ItemRevision(1))),
        Some(SubagentRunId(Base64UUID.zero)),
        DelegateCouplingState.Coupled,
        1001L,
        Some(Problem("PROBLEM"))),
      json"""{
        "subagentItem": {
          "agentPath": "AGENT",
          "disabled": false,
          "id": "SUBAGENT",
          "itemRevision": 1,
          "uri": "https://example.com"
        },
        "subagentRunId": "AAAAAAAAAAAAAAAAAAAAAA",
        "couplingState": {
          "TYPE": "Coupled"
        },
        "eventId": 1001,
        "problem": {
          "message": "PROBLEM"
        }
      }""")
  }
}