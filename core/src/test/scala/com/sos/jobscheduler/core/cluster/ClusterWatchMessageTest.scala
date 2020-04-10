package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.cluster.ClusterEvent.ClusterNodesAppointed
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterState}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchMessageTest extends AnyFreeSpec
{
  "JSON" - {
    "ClusterWatchEvents" in {
      testJson[ClusterWatchMessage](
        ClusterWatchEvents(
          from = ClusterNodeId("A"),
          List(
            ClusterNodesAppointed(
              Map(
                ClusterNodeId("A") -> Uri("http://A"),
                ClusterNodeId("B") -> Uri("http://B")),
              ClusterNodeId("A"))),
          ClusterState.NodesAppointed(
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
              "TYPE": "ClusterNodesAppointed",
              "idToUri": {
                "A": "http://A",
                "B": "http://B"
              },
              "activeId": "A"
            }
          ],
          "clusterState": {
            "TYPE": "NodesAppointed",
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
          ClusterState.Coupled(
            Map(
              ClusterNodeId("A") -> Uri("http://A"),
              ClusterNodeId("B") -> Uri("http://B")),
            ClusterNodeId("A"))),
        json"""{
          "TYPE": "ClusterWatchHeartbeat",
          "from": "A",
          "clusterState": {
            "TYPE": "Coupled",
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
