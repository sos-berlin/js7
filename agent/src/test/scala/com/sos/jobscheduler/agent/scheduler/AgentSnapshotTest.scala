package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentSnapshotTest extends FreeSpec {

  "JSON" in {
    implicit val codec = AgentSnapshot.jsonCodec
    testJson[Any](AgentSnapshot.Master(MasterId("MASTER")), """
      {
        "TYPE": "Master",
        "masterId": "MASTER"
      }""")
  }
}
