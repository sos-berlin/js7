package js7.core.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.ClusterState
import js7.data.node.NodeId
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
          from = NodeId("A"),
          List(
            ClusterNodesAppointed(
              Map(
                NodeId("A") -> Uri("http://A"),
                NodeId("B") -> Uri("http://B")),
              NodeId("A"))),
          ClusterState.NodesAppointed(
            Map(
              NodeId("A") -> Uri("http://A"),
              NodeId("B") -> Uri("http://B")),
            NodeId("A")),
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
          from = NodeId("A"),
          ClusterState.Coupled(
            Map(
              NodeId("A") -> Uri("http://A"),
              NodeId("B") -> Uri("http://B")),
            NodeId("A"))),
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
