package js7.core.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.{ClusterSetting, ClusterState}
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
              ClusterSetting(
                Map(
                  NodeId("A") -> Uri("http://A"),
                  NodeId("B") -> Uri("http://B")),
                NodeId("A")))),
          ClusterState.NodesAppointed(
            ClusterSetting(
              Map(
                NodeId("A") -> Uri("http://A"),
                NodeId("B") -> Uri("http://B")),
              NodeId("A"))),
          force = true),
        json"""{
          "TYPE": "ClusterWatchEvents",
          "from": "A",
          "events": [
            {
              "TYPE": "ClusterNodesAppointed",
              "setting": {
                "idToUri": {
                  "A": "http://A",
                  "B": "http://B"
                },
                "activeId": "A"
              }
            }
          ],
          "clusterState": {
            "TYPE": "NodesAppointed",
              "setting": {
              "idToUri": {
                "A": "http://A",
                "B": "http://B"
              },
              "activeId": "A"
            }
          },
          "force": true
        }""")
    }

    "ClusterWatchHeartbeat" in {
      testJson[ClusterWatchMessage](
        ClusterWatchHeartbeat(
          from = NodeId("A"),
          ClusterState.Coupled(
            ClusterSetting(
              Map(
                NodeId("A") -> Uri("http://A"),
                NodeId("B") -> Uri("http://B")),
              NodeId("A")))),
        json"""{
          "TYPE": "ClusterWatchHeartbeat",
          "from": "A",
          "clusterState": {
            "TYPE": "Coupled",
            "setting": {
              "idToUri": {
                "A": "http://A",
                "B": "http://B"
              },
              "activeId": "A"
            }
          }
        }""")
    }
  }
}
