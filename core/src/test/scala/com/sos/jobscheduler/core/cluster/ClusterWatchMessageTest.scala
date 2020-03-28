package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterEvent.NodesAppointed
import com.sos.jobscheduler.data.cluster.ClusterNodeId
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
          from = ClusterNodeId("A"),
          List(
            NodesAppointed(
              Map(
                ClusterNodeId("A") -> Uri("http://A"),
                ClusterNodeId("B") -> Uri("http://B")),
              ClusterNodeId("A"))),
          ClusterNodesAppointed(
            Map(
              ClusterNodeId("A") -> Uri("http://A"),
              ClusterNodeId("B") -> Uri("http://B")),
            ClusterNodeId("A")),
          force = true),
        json"""{
          "TYPE": "ClusterWatchEvents",
          "from": "A",
          "events": [
            {
              "TYPE": "Cluster.NodesAppointed",
              "idToUri": {
                "A": "http://A",
                "B": "http://B"
              },
              "activeId": "A"
            }
          ],
          "clusterState": {
            "TYPE": "ClusterNodesAppointed",
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          },
          "force": true
        }""")
    }

    "ClusterWatchHeartbeat" in {
      testJson[ClusterWatchMessage](
        ClusterWatchHeartbeat(
          from = ClusterNodeId("A"),
          ClusterCoupled(
            Map(
              ClusterNodeId("A") -> Uri("http://A"),
              ClusterNodeId("B") -> Uri("http://B")),
            ClusterNodeId("A"))),
        json"""{
          "TYPE": "ClusterWatchHeartbeat",
          "from": "A",
          "clusterState": {
            "TYPE": "ClusterCoupled",
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          }
        }""")
    }
  }
}
