package js7.controller.data.agent

import java.util.UUID
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.web.Uri
import js7.data.agent.{AgentId, AgentRef, AgentRunId}
import js7.data.event.JournalId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AgentRefStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[AgentRefState](
      AgentRefState(
        AgentRef(AgentId("AGENT"), Uri("https://URI")),
        None,
        None,
        AgentRefState.Decoupled,
        123L),
      json"""{
        "agentRef": {
          "id": "AGENT",
          "uri": "https://URI"
        },
        "couplingState": {
          "TYPE": "Decoupled"
        },
        "eventId": 123
      }""")

    val agentRunId = AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))

    testJson[AgentRefState](
      AgentRefState(
        AgentRef(AgentId("AGENT"), Uri("https://URI")),
        Some(agentRunId),
        None,
        AgentRefState.Decoupled,
        123L),
      json"""{
        "agentRef": {
          "id": "AGENT",
          "uri": "https://URI"
        },
        "agentRunId": "ABEiM0RVZneImaq7zN3u_w",
        "couplingState": {
          "TYPE": "Decoupled"
        },
        "eventId": 123
      }""")

    testJson[AgentRefState.CouplingState](AgentRefState.Decoupled, json"""{ "TYPE": "Decoupled"}""")
    testJson[AgentRefState.CouplingState](AgentRefState.Coupled, json"""{ "TYPE": "Coupled"}""")

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
