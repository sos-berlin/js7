package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.time.ZoneId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterAgentEventTest extends FreeSpec
{
  "MasterAgentEvent" in {
    testJson[KeyedEvent[MasterAgentEvent]](AgentPath("/AGENT") <-: MasterAgentEvent.AgentReady(ZoneId.of("Europe/Berlin")),
      json"""{
        "TYPE": "AgentReady",
        "key": "/AGENT",
        "timezone": "Europe/Berlin"
      }""")
  }
}
