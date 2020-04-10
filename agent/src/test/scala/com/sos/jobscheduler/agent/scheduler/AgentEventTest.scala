package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.event.{JournalId, KeyedEvent}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.util.UUID
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentEventTest extends AnyFreeSpec {

  "JSON" - {
    "MasterRegistered" in {
      testJson[KeyedEvent[AgentEvent]](
        AgentEvent.MasterRegistered(MasterId("MASTER"), AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))),
        json""" {
          "TYPE": "MasterRegistered",
          "masterId": "MASTER",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
        }""")
    }
  }
}
