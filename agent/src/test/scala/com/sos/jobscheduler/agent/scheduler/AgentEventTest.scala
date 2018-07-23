package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentEventTest extends FreeSpec {

  "JSON" in {
    testJson[KeyedEvent[AgentEvent]](AgentEvent.MasterAdded(MasterId("MASTER")), """
      {
        "TYPE": "MasterAdded",
        "masterId": "MASTER"
      }""")
  }
}
