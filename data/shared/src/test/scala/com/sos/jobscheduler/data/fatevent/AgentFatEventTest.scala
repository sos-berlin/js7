package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentPath
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
    testJson[KeyedEvent[AgentFatEvent]](AgentPath("/AGENT") <-: AgentFatEvent.AgentReadyFat(ZoneId.of("Europe/Berlin")),
      json"""{
        "TYPE": "AgentReadyFat",
        "key": "/AGENT",
        "timezone": "Europe/Berlin"
      }""")
  }
}
