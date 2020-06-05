package js7.master.data.events

import java.util.UUID
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.{JournalId, KeyedEvent}
import js7.master.data.events.MasterAgentEvent.{AgentCouplingFailed, AgentReady, AgentRegisteredMaster}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterAgentEventTest extends AnyFreeSpec
{
  "JSON" - {
    "AgentCouplingFailed" in {
      testJson[KeyedEvent[MasterAgentEvent]](AgentRefPath("/AGENT") <-: AgentCouplingFailed(Problem("ERROR")),
        json"""{
          "TYPE": "AgentCouplingFailed",
          "key": "/AGENT",
          "problem": {
            "message": "ERROR"
          }
        }""")
    }

    "AgentReady" in {
      testJson[KeyedEvent[MasterAgentEvent]](AgentRefPath("/AGENT") <-: AgentReady("Europe/Berlin"),
        json"""{
          "TYPE": "AgentReady",
          "key": "/AGENT",
          "timezone": "Europe/Berlin"
        }""")
    }

    "AgentRegisteredMaster" in {
      testJson[KeyedEvent[MasterAgentEvent]](
        AgentRefPath("/AGENT") <-: AgentRegisteredMaster(AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))),
        json"""{
          "TYPE": "AgentRegisteredMaster",
          "key": "/AGENT",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
        }""")
    }
  }
}
