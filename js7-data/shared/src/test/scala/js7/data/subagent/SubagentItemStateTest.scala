package js7.data.subagent

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.utils.Base64UUID
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.delegate.DelegateCouplingState
import js7.data.item.ItemRevision
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
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
        isDetaching = false,
        isResettingForcibly = None,
        eventId = 1001L,
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

    testJson[SubagentItemState](
      SubagentItemState(
        SubagentItem(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          Uri("https://example.com"),
          itemRevision = Some(ItemRevision(1))),
        None,
        DelegateCouplingState.Coupled,
        /*Agent only*/isDetaching = true,
        /*Controller only*/isResettingForcibly = Some(false),
        1001L,
        None),
      json"""{
        "subagentItem": {
          "agentPath": "AGENT",
          "disabled": false,
          "id": "SUBAGENT",
          "itemRevision": 1,
          "uri": "https://example.com"
        },
        "couplingState": {
          "TYPE": "Coupled"
        },
        "isDetaching": true,
        "isResettingForcibly": false,
        "eventId": 1001
      }""")

    // COMPATIBLE with v2.2.2
    testJsonDecoder[SubagentItemState](
      SubagentItemState(
        SubagentItem(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          Uri("https://example.com"),
          itemRevision = Some(ItemRevision(1))),
        Some(SubagentRunId(Base64UUID.zero)),
        DelegateCouplingState.Coupled,
        eventId = 1001L,
        problem = Some(Problem("PROBLEM"))),
      json"""{
        "subagentRef": {
          "agentPath": "AGENT",
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
