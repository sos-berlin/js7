package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.time.ZoneId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentFatEventTest extends FreeSpec
{
  "MasterReadyFat" in {
    testJson[KeyedEvent[AgentFatEvent]](AgentRefPath("/AGENT") <-: AgentFatEvent.AgentReadyFat(ZoneId.of("Europe/Berlin").getId),
      json"""{
        "TYPE": "AgentReadyFat",
        "key": "/AGENT",
        "timezone": "Europe/Berlin"
      }""")
  }
}
