package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentFatEventTest extends AnyFreeSpec
{
  "MasterReadyFat" in {
    testJson[KeyedEvent[AgentFatEvent]](AgentRefPath("/AGENT") <-: AgentFatEvent.AgentReadyFat("Europe/Berlin"),
      json"""{
        "TYPE": "AgentReadyFat",
        "key": "/AGENT",
        "timezone": "Europe/Berlin"
      }""")
  }
}
