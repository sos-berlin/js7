package js7.data.cluster

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.log.CorrelId
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.{ClusterNodesAppointed, ClusterPassiveLost}
import js7.data.cluster.ClusterState.NodesAppointed
import js7.data.cluster.ClusterWatchRequest.RequestId
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson

final class ClusterWatchRequestTest extends OurTestSuite:

  "JSON" - {
    val clusterSetting = ClusterSetting(
      Map(
        NodeId("A") -> Uri("https://A"),
        NodeId("B") -> Uri("https://B")),
      NodeId("A"),
      ClusterTiming(10.s, 20.s))

    "ClusterWatchCheckEvent" in:
      testJson[ClusterWatchRequest](
        ClusterWatchCheckEvent(
          RequestId(123),
          CorrelId("TESTTEST"),
          from = NodeId("A"),
          ClusterNodesAppointed(clusterSetting),
          NodesAppointed(clusterSetting),
          forceWhenUntaught = false),
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
          "forceWhenUntaught": false
        }""")

    "ClusterWatchAskNodeLoss" in:
      testJson[ClusterWatchRequest](
        ClusterWatchAskNodeLoss(
          RequestId(123),
          CorrelId("TESTTEST"),
          from = NodeId("A"),
          ClusterPassiveLost(NodeId.primary),
          ClusterState.PassiveLost(clusterSetting),
          hold = 7.s,
          forceWhenUntaught = true),
        json"""{
          "TYPE": "ClusterWatchAskNodeLoss",
          "requestId": 123,
          "correlId": "TESTTEST",
          "from": "A",
          "event": {
            "TYPE": "ClusterPassiveLost",
            "id": "Primary"
          },
          "clusterState": {
            "TYPE": "PassiveLost",
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
          "hold": 7,
          "forceWhenUntaught": true
        }""")

    "ClusterWatchCommitNodeLoss" in:
      testJson[ClusterWatchRequest](
        ClusterWatchCommitNodeLoss(
          RequestId(123),
          CorrelId("TESTTEST"),
          from = NodeId("A"),
          ClusterPassiveLost(NodeId.primary),
          ClusterState.PassiveLost(clusterSetting),
          forceWhenUntaught = true),
        json"""{
          "TYPE": "ClusterWatchCommitNodeLoss",
          "requestId": 123,
          "correlId": "TESTTEST",
          "from": "A",
          "event": {
            "TYPE": "ClusterPassiveLost",
            "id": "Primary"
          },
          "clusterState": {
            "TYPE": "PassiveLost",
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
          "forceWhenUntaught": true
        }""")

    "ClusterWatchCheckState" in:
      testJson[ClusterWatchRequest](
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
