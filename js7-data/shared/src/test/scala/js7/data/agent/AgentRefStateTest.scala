package js7.data.agent

import java.util.UUID
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.event.JournalId
import js7.data.item.ItemRevision
import js7.data.subagent.SubagentId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AgentRefStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      AgentRefState(
        AgentRef(
          AgentPath("AGENT"),
          directors = Seq(SubagentId("SUBAGENT-1")),
          itemRevision = Some(ItemRevision(0))),
        None,
        None,
        DelegateCouplingState.Reset,
        123L,
        None),
      json"""{
        "agentRef": {
          "path": "AGENT",
          "directors": [ "SUBAGENT-1" ],
          "itemRevision": 0
        },
        "couplingState": {
          "TYPE": "Reset"
        },
        "eventId": 123
      }""")

    val agentRunId = AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))

    testJson(
      AgentRefState(
        AgentRef(
          AgentPath("AGENT"),
          directors = Seq(SubagentId("SUBAGENT-1")),
          itemRevision = Some(ItemRevision(0))),
        Some(agentRunId),
        Some("UTC"),
        DelegateCouplingState.Resetting(force = true),
        123L,
        Some(Problem("PROBLEM"))),
      json"""{
        "agentRef": {
          "path": "AGENT",
          "directors": [ "SUBAGENT-1" ],
          "itemRevision": 0
        },
        "agentRunId": "ABEiM0RVZneImaq7zN3u_w",
        "timezone": "UTC",
        "couplingState": {
          "TYPE": "Resetting",
          "force": true
        },
        "eventId": 123,
        "problem": {
          "message": "PROBLEM"
        }
      }""")
  }
}
