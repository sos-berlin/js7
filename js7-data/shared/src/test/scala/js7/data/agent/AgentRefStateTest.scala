package js7.data.agent

import java.util.UUID
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.event.JournalId
import js7.data.item.ItemRevision
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AgentRefStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      AgentRefState(
        AgentRef(AgentPath("AGENT"), Uri("https://URI"), Some(ItemRevision(0))),
        None,
        None,
        AgentRefState.Reset,
        123L),
      json"""{
        "agentRef": {
          "path": "AGENT",
          "uri": "https://URI",
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
        AgentRef(AgentPath("AGENT"), Uri("https://URI"), Some(ItemRevision(0))),
        Some(agentRunId),
        None,
        AgentRefState.Reset,
        123L),
      json"""{
        "agentRef": {
          "path": "AGENT",
          "uri": "https://URI",
          "itemRevision": 0
        },
        "agentRunId": "ABEiM0RVZneImaq7zN3u_w",
        "couplingState": {
          "TYPE": "Reset"
        },
        "eventId": 123
      }""")

    testJson[AgentRefState.CouplingState](AgentRefState.Reset, json"""{ "TYPE": "Reset" }""")
    testJson[AgentRefState.CouplingState](AgentRefState.Coupled, json"""{ "TYPE": "Coupled" }""")

    testJson[AgentRefState.CouplingState](
      AgentRefState.CouplingFailed(Problem("PROBLEM")),
      json"""{
        "TYPE": "CouplingFailed",
        "problem": {
          "message": "PROBLEM"
        }
      }""")
  }
}
