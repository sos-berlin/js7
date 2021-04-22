package js7.data.agent

import java.util.UUID
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentEventsObserved, AgentReady, AgentRegisteredController}
import js7.data.event.{JournalId, KeyedEvent}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AgentRefStateEventTest extends AnyFreeSpec
{
  "JSON" - {
    "AgentCouplingFailed" in {
      testJson[KeyedEvent[AgentRefStateEvent]](AgentPath("AGENT") <-: AgentCouplingFailed(Problem("ERROR")),
        json"""{
          "TYPE": "AgentCouplingFailed",
          "Key": "AGENT",
          "problem": {
            "message": "ERROR"
          }
        }""")
    }

    "AgentReady" in {
      testJson[KeyedEvent[AgentRefStateEvent]](AgentPath("AGENT") <-: AgentReady("Europe/Berlin"),
        json"""{
          "TYPE": "AgentReady",
          "Key": "AGENT",
          "timezone": "Europe/Berlin"
        }""")
    }

    "AgentRegisteredController" in {
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentRegisteredController(AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))),
        json"""{
          "TYPE": "AgentRegisteredController",
          "Key": "AGENT",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
        }""")
    }

    "AgentEventsObserved" in {
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentEventsObserved(123L),json"""
        {
          "TYPE":  "AgentEventsObserved",
          "Key": "AGENT",
          "untilEventId": 123
        }""")
    }
  }
}
