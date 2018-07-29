package com.sos.jobscheduler.agent.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.time.ZoneId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentMasterEventTest extends FreeSpec
{
  "MasterReadyFat" in {
    testJson[KeyedEvent[AgentMasterEvent]](AgentMasterEvent.AgentReadyForMaster(ZoneId.of("Europe/Berlin")),
      json"""{
        "TYPE": "AgentReadyForMaster",
        "timezone": "Europe/Berlin"
      }""")
  }
}
