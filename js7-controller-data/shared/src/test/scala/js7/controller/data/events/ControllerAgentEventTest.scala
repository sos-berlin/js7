package js7.controller.data.events

import java.util.UUID
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.controller.data.events.ControllerAgentEvent.{AgentCouplingFailed, AgentReady, AgentRegisteredController}
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.{JournalId, KeyedEvent}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerAgentEventTest extends AnyFreeSpec
{
  "JSON" - {
    "AgentCouplingFailed" in {
      testJson[KeyedEvent[ControllerAgentEvent]](AgentRefPath("/AGENT") <-: AgentCouplingFailed(Problem("ERROR")),
        json"""{
          "TYPE": "AgentCouplingFailed",
          "key": "/AGENT",
          "problem": {
            "message": "ERROR"
          }
        }""")
    }

    "AgentReady" in {
      testJson[KeyedEvent[ControllerAgentEvent]](AgentRefPath("/AGENT") <-: AgentReady("Europe/Berlin"),
        json"""{
          "TYPE": "AgentReady",
          "key": "/AGENT",
          "timezone": "Europe/Berlin"
        }""")
    }

    "AgentRegisteredController" in {
      testJson[KeyedEvent[ControllerAgentEvent]](
        AgentRefPath("/AGENT") <-: AgentRegisteredController(AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))),
        json"""{
          "TYPE": "AgentRegisteredController",
          "key": "/AGENT",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
        }""")
    }
  }
}
