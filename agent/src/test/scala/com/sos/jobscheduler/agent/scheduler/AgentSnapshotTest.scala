package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentSnapshotTest extends FreeSpec {

  "JSON" in {
    implicit val codec = AgentSnapshot.jsonCodec
    testJson[Any](AgentSnapshot.Master(UserId.Anonymous), """
      {
        "TYPE": "Master",
        "userId": "Anonymous"
      }""")
  }
}
