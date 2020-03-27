package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterEvent.NodesAppointed
import com.sos.jobscheduler.data.cluster.ClusterState.{ClusterCoupled, ClusterNodesAppointed}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchMessageTest extends FreeSpec
{
  "JSON" - {
    "ClusterWatchEvents" in {
      testJson[ClusterWatchMessage](
        ClusterWatchEvents(
          Uri("http://PRIMARY"),
          NodesAppointed(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil) :: Nil,
          ClusterNodesAppointed(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil),
          force = true),
        json"""{
          "TYPE": "ClusterWatchEvents",
          "from": "http://PRIMARY",
          "events": [
            {
              "TYPE": "Cluster.NodesAppointed",
              "uris": [ "http://PRIMARY", "http://BACKUP" ]
            }
          ],
          "clusterState": {
            "TYPE": "ClusterNodesAppointed",
            "uris": [ "http://PRIMARY", "http://BACKUP" ]
          },
          "force": true
        }""")
    }

    "ClusterWatchHeartbeat" in {
      testJson[ClusterWatchMessage](
        ClusterWatchHeartbeat(
          Uri("http://PRIMARY"),
          ClusterCoupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 1)),
        json"""{
          "TYPE": "ClusterWatchHeartbeat",
          "from": "http://PRIMARY",
          "clusterState": {
            "TYPE": "ClusterCoupled",
            "uris": [ "http://PRIMARY", "http://BACKUP" ],
            "active": 1
          }
        }""")
    }
  }
}
