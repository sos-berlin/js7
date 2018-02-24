package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentEventTest extends FreeSpec {

  "JSON" in {
    testJson[KeyedEvent[AgentEvent]](KeyedEvent(AgentEvent.MasterAdded)(UserId.Anonymous), """
      {
        "TYPE": "MasterAdded",
        "key": "Anonymous"
      }""")
  }
}
