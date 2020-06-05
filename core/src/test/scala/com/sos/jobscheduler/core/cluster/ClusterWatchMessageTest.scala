package js7.core.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.{ClusterNodeId, ClusterState}
import js7.tester.CirceJsonTester.testJson
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
