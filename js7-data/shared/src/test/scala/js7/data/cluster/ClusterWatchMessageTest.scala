package js7.data.cluster

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.log.CorrelId
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.ClusterState.NodesAppointed
import js7.data.cluster.ClusterWatchMessage.RequestId
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson

final class ClusterWatchMessageTest extends OurTestSuite
{
  "JSON" - {
    val clusterSetting = ClusterSetting(
      Map(
        NodeId("A") -> Uri("https://A"),
        NodeId("B") -> Uri("https://B")),
      NodeId("A"),
      ClusterTiming(10.s, 20.s))

    "ClusterWatchCheckEvent" in {
      testJson[ClusterWatchMessage](
        ClusterWatchCheckEvent(
          RequestId(123),
          CorrelId("TESTTEST"),
          from = NodeId("A"),
          ClusterNodesAppointed(clusterSetting),
          NodesAppointed(clusterSetting)),
        json"""{
          "TYPE": "ClusterWatchCheckEvent",
          "requestId": 123,
          "correlId": "TESTTEST",
          "from": "A",
          "event": {
            "TYPE": "ClusterNodesAppointed",
            "setting": {
              "idToUri": {
                "A": "https://A",
                "B": "https://B"
              },
              "activeId": "A",
              "timing": {
                "heartbeat": 10,
                "heartbeatTimeout": 20
              }
            }
          },
          "clusterState": {
            "TYPE": "NodesAppointed",
              "setting": {
              "idToUri": {
                "A": "https://A",
                "B": "https://B"
              },
              "activeId": "A",
              "timing": {
                "heartbeat": 10,
                "heartbeatTimeout": 20
              }
            }
          },
          "checkOnly": false
        }""")
    }

    "ClusterWatchCheckState" in {
      testJson[ClusterWatchMessage](
        ClusterWatchCheckState(
          RequestId(123),
          CorrelId("TESTTEST"),
          from = NodeId("A"),
          NodesAppointed(clusterSetting)),
        json"""{
          "TYPE": "ClusterWatchCheckState",
          "requestId": 123,
          "correlId": "TESTTEST",
          "from": "A",
          "clusterState": {
            "TYPE": "NodesAppointed",
            "setting": {
              "idToUri": {
                "A": "https://A",
                "B": "https://B"
              },
              "activeId": "A",
              "timing": {
                "heartbeat": 10,
                "heartbeatTimeout": 20
              }
            }
          }
        }""")
    }

    "ClusterWatchHeartbeat" in {
      testJson[ClusterWatchMessage](
        ClusterWatchHeartbeat(
          CorrelId("TESTTEST"),
          from = NodeId("A"),
          NodesAppointed(clusterSetting)),
        json"""{
          "TYPE": "ClusterWatchHeartbeat",
          "correlId": "TESTTEST",
          "from": "A",
          "clusterState": {
            "TYPE": "NodesAppointed",
            "setting": {
              "idToUri": {
                "A": "https://A",
                "B": "https://B"
              },
              "activeId": "A",
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