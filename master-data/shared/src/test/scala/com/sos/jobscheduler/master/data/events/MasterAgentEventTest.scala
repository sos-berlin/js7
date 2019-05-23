package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.event.{JournalId, KeyedEvent}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.{AgentCouplingFailed, AgentReady, AgentRegisteredMaster}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.time.ZoneId
import java.util.UUID
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterAgentEventTest extends FreeSpec
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
      testJson[KeyedEvent[MasterAgentEvent]](AgentRefPath("/AGENT") <-: AgentReady(ZoneId.of("Europe/Berlin").getId),
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
