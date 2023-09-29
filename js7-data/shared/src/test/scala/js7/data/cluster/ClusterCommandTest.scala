package js7.data.cluster

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.OneTimeToken
import js7.base.web.Uri
import js7.data.cluster.ClusterCommand.*
import js7.data.cluster.ClusterState.FailedOver
import js7.data.event.JournalPosition
import js7.data.node.{NodeId, NodeName}
import js7.tester.CirceJsonTester.testJson

final class ClusterCommandTest extends OurTestSuite:
  "ClusterStartBackupNode" in:
    testJson[ClusterCommand](
      ClusterStartBackupNode(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("https://A"),
            NodeId("B") -> Uri("https://B")),
          NodeId("A"),
          ClusterTiming(10.s, 20.s),
          Some(ClusterWatchId("CLUSTER-WATCH"))),
        1000L,
        NodeName("ActiveNodeName"),
        UserId("PassiveNodeUserId")),
      json"""{
        "TYPE": "ClusterStartBackupNode",
        "setting": {
          "idToUri": {
            "A": "https://A",
            "B": "https://B"
          },
          "activeId": "A",
          "timing": {
            "heartbeat": 10,
            "heartbeatTimeout": 20
          },
          "clusterWatchId": "CLUSTER-WATCH"
        },
        "fileEventId": 1000,
        "activeNodeName": "ActiveNodeName",
        "passiveNodeUserId": "PassiveNodeUserId"
      }""")

  "ClusterPrepareCoupling" in:
    testJson[ClusterCommand](
      ClusterPrepareCoupling(NodeId("A"), NodeId("B"), OneTimeToken("TOKEN")),
      json"""{
        "TYPE": "ClusterPrepareCoupling",
        "activeId": "A",
        "passiveId": "B",
        "token": "TOKEN"
      }""")

  "ClusterCouple" in:
    testJson[ClusterCommand](
      ClusterCouple(NodeId("A"), NodeId("B"), OneTimeToken("TOKEN")),
      json"""{
        "TYPE": "ClusterCouple",
        "activeId": "A",
        "passiveId": "B",
        "token": "TOKEN"
      }""")

  "ClusterConfirmCoupling" in:
    testJson[ClusterCommand](
      ClusterConfirmCoupling(OneTimeToken("TOKEN")),
      json"""{
        "TYPE": "ClusterConfirmCoupling",
        "token": "TOKEN"
      }""")

  "ClusterRecouple" in:
    testJson[ClusterCommand](
      ClusterRecouple(NodeId("A"), NodeId("B")),
      json"""{
        "TYPE": "ClusterRecouple",
        "activeId": "A",
        "passiveId": "B"
      }""")

  "ClusterInhibitActivation" in:
    testJson[ClusterCommand](ClusterInhibitActivation(7.s),
      json"""{
        "TYPE": "ClusterInhibitActivation",
        "duration": 7
      }""")

  "ClusterInhibitActivation.Response" in:
    testJson[ClusterCommand.Response](ClusterInhibitActivation.Response(Some(FailedOver(
      ClusterSetting(
        Map(
          NodeId("A") -> Uri("https://A"),
          NodeId("B") -> Uri("https://B")),
        activeId = NodeId("A"),
        ClusterTiming(10.s, 20.s),
        None),
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

  "Response.Accepted" in:
    testJson[ClusterCommand.Response](
      Response.Accepted,
      json"""{
        "TYPE": "Accepted"
      }""")
