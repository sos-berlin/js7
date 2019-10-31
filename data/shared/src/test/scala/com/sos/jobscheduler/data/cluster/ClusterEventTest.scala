package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, ClusterCoupled, FollowingStarted}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterEventTest extends FreeSpec
{
  "BecameSole" in {
    testJson[ClusterEvent](BecameSole(ClusterNodeId("ACTIVE")),
      json"""{
        "TYPE": "BecameSole",
        "activeNodeId": "ACTIVE"
      }""")
  }

  "BackupNodeAppointed" in {
    testJson[ClusterEvent](BackupNodeAppointed(ClusterNodeId("PASSIVE"), Uri("http://example.com")),
      json"""{
        "TYPE": "BackupNodeAppointed",
        "nodeId": "PASSIVE",
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

  "ClusterCoupled" in {
    testJson[ClusterEvent](ClusterCoupled,
      json"""{
        "TYPE": "ClusterCoupled"
      }""")
  }
}
