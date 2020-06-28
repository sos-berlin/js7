package js7.data.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.data.cluster.ClusterCommand._
import js7.data.cluster.ClusterState.FailedOver
import js7.data.event.JournalPosition
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class ClusterCommandTest extends AnyFreeSpec
{
  "ClusterStartBackupNode" in {
    testJson[ClusterCommand](
      ClusterStartBackupNode(
        Map(
          NodeId("A") -> Uri("http://A"),
          NodeId("B") -> Uri("http://B")),
        NodeId("A"),
        1000L),
      json"""{
        "TYPE": "ClusterStartBackupNode",
        "idToUri": {
          "A": "http://A",
          "B": "http://B"
        },
        "activeId": "A",
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
      Map(
        NodeId("A") -> Uri("http://A"),
        NodeId("B") -> Uri("http://B")),
      activeId = NodeId("A"),
      JournalPosition(0L, 1000)))),
      json"""{
        "TYPE": "ClusterInhibitActivation.Response",
        "failedOver": {
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A",
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
