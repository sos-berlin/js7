package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentSnapshotTest extends FreeSpec {

  "JSON" in {
    implicit val codec = AgentSnapshot.jsonCodec
    testJson[Any](AgentSnapshot.Master(MasterId("MASTER"), AgentRunId("RUN-ID")), json"""
      {
        "TYPE": "Master",
        "masterId": "MASTER",
        "agentRunId": "RUN-ID"
      }""")
  }
}
