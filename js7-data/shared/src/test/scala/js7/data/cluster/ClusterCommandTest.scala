package js7.data.cluster

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.cluster.ClusterCommand.*
import js7.data.cluster.ClusterState.FailedOver
import js7.data.event.JournalPosition
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson

final class ClusterCommandTest extends OurTestSuite
{
  "ClusterStartBackupNode" in {
    testJson[ClusterCommand](
      ClusterStartBackupNode(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("https://A"),
            NodeId("B") -> Uri("https://B")),
          NodeId("A"),
          ClusterTiming(10.s, 20.s),
          Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH")))),
        1000L),
      json"""{
        "TYPE": "ClusterStartBackupNode",
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
        },
        "fileEventId": 1000
      }""")
  }

  "ClusterPrepareCoupling" in {
    testJson[ClusterCommand](
      ClusterPrepareCoupling(NodeId("A"), NodeId("B")),
      json"""{
        "TYPE": "ClusterPrepareCoupling",
        "activeId": "A",
        "passiveId": "B"
      }""")
  }

  "ClusterCouple" in {
    testJson[ClusterCommand](
      ClusterCouple(NodeId("A"), NodeId("B")),
      json"""{
        "TYPE": "ClusterCouple",
        "activeId": "A",
        "passiveId": "B"
      }""")
  }

  "ClusterRecouple" in {
    testJson[ClusterCommand](
      ClusterRecouple(NodeId("A"), NodeId("B")),
      json"""{
        "TYPE": "ClusterRecouple",
        "activeId": "A",
        "passiveId": "B"
      }""")
  }

  "ClusterInhibitActivation" in {
    testJson[ClusterCommand](ClusterInhibitActivation(7.s),
      json"""{
        "TYPE": "ClusterInhibitActivation",
        "duration": 7
      }""")
  }

  "ClusterInhibitActivation.Response" in {
    testJson[ClusterCommand.Response](ClusterInhibitActivation.Response(Some(FailedOver(
      ClusterSetting(
        Map(
          NodeId("A") -> Uri("https://A"),
          NodeId("B") -> Uri("https://B")),
        activeId = NodeId("A"),
        ClusterTiming(10.s, 20.s),
        Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH")))),
      JournalPosition(0L, 1000)))),
      json"""{
        "TYPE": "ClusterInhibitActivation.Response",
        "failedOver": {
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
          },
          "failedAt": {
            "fileEventId": 0,
            "position": 1000
          }
        }
      }""")
  }

  "Response.Accepted" in {
    testJson[ClusterCommand.Response](
      Response.Accepted,
      json"""{
        "TYPE": "Accepted"
      }""")
  }
}
