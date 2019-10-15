package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, FollowingStarted}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterEventTest extends FreeSpec
{
  "BackupNodeAppointed" in {
    testJson[ClusterEvent](BackupNodeAppointed(ClusterNodeId("NODE-ID"), Uri("http://example.com")),
      json"""{
        "TYPE": "BackupNodeAppointed",
        "nodeId": "NODE-ID",
        "uri": "http://example.com"
      }""")
  }

  "FollowingStarted" in {
    testJson[ClusterEvent](FollowingStarted(ClusterNodeId("FOLLOWER"), Uri("http://example.com")),
      json"""{
        "TYPE": "FollowingStarted",
        "passiveNodeId": "FOLLOWER",
        "activeUri": "http://example.com"
      }""")
  }
}
