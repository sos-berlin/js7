package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.event.JournalId
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.util.UUID
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentSnapshotTest extends FreeSpec {

  "JSON" in {
    implicit val codec = AgentSnapshot.jsonCodec
    testJson[Any](
      AgentSnapshot.Master(MasterId("MASTER"), AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))),
      json""" {
        "TYPE": "Master",
        "masterId": "MASTER",
        "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
      }""")
  }
}
