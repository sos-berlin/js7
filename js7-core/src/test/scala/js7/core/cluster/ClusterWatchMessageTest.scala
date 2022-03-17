package js7.core.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming}
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
                  NodeId("A") -> Uri("https://A"),
                  NodeId("B") -> Uri("https://B")),
                NodeId("A"),
                Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
                ClusterTiming(10.s, 20.s)))),
          ClusterState.NodesAppointed(
            ClusterSetting(
              Map(
                NodeId("A") -> Uri("https://A"),
                NodeId("B") -> Uri("https://B")),
              NodeId("A"),
              Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
              ClusterTiming(10.s, 20.s)))),
        json"""{
          "TYPE": "ClusterWatchEvents",
          "from": "A",
          "events": [
            {
              "TYPE": "ClusterNodesAppointed",
              "setting": {
                "idToUri": {
                  "A": "https://A",
                  "B": "https://B"
                },
                "activeId": "A",
                "clusterWatches": [ { "uri": "https://CLUSTER-WATCH" } ],
                "timing": {
                  "heartbeat": 10,
                  "heartbeatTimeout": 20
                }
              }
            }
          ],
          "clusterState": {
            "TYPE": "NodesAppointed",
              "setting": {
              "idToUri": {
                "A": "https://A",
                "B": "https://B"
              },
              "activeId": "A",
              "clusterWatches": [ { "uri": "https://CLUSTER-WATCH" } ],
              "timing": {
                "heartbeat": 10,
                "heartbeatTimeout": 20
              }
            }
          },
          "checkOnly": false
        }""")
    }

    "ClusterWatchHeartbeat" in {
      testJson[ClusterWatchMessage](
        ClusterWatchHeartbeat(
          from = NodeId("A"),
          ClusterState.Coupled(
            ClusterSetting(
              Map(
                NodeId("A") -> Uri("https://A"),
                NodeId("B") -> Uri("https://B")),
              NodeId("A"),
              Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
              ClusterTiming(10.s, 20.s)))),
        json"""{
          "TYPE": "ClusterWatchHeartbeat",
          "from": "A",
          "clusterState": {
            "TYPE": "Coupled",
            "setting": {
              "idToUri": {
                "A": "https://A",
                "B": "https://B"
              },
              "activeId": "A",
              "clusterWatches": [ { "uri": "https://CLUSTER-WATCH" } ],
              "timing": {
                "heartbeat": 10,
                "heartbeatTimeout": 20
              }
            }
          }
        }""")
    }
  }
}
