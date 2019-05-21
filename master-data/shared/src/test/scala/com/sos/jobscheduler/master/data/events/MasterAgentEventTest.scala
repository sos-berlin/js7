package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.{AgentCouplingFailed, AgentReady}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.time.ZoneId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterAgentEventTest extends FreeSpec
{
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
}
