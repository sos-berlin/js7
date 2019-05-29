package com.sos.jobscheduler.agent.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AgentMasterEventTest extends FreeSpec
{
  "AgentReadyForMaster" in {
    testJson[KeyedEvent[AgentMasterEvent]](AgentMasterEvent.AgentReadyForMaster("Europe/Berlin", 1.hour),
      json"""{
        "TYPE": "AgentReadyForMaster",
        "timezone": "Europe/Berlin",
        "totalRunningTime": 3600
      }""")
  }
}
