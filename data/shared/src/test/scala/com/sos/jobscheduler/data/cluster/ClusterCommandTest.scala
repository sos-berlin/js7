package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.cluster.ClusterCommand._
import com.sos.jobscheduler.data.cluster.ClusterState.FailedOver
import com.sos.jobscheduler.data.event.JournalPosition
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

final class ClusterCommandTest extends FreeSpec
{
  "ClusterStartBackupNode" in {
    testJson[ClusterCommand](
      ClusterStartBackupNode(
        Map(
          ClusterNodeId("A") -> Uri("http://A"),
          ClusterNodeId("B") -> Uri("http://B")),
        ClusterNodeId("A")),
      json"""{
        "TYPE": "ClusterStartBackupNode",
        "idToUri": {
          "A": "http://A",
          "B": "http://B"
        },
        "activeId": "A"
      }""")
  }

  "ClusterPrepareCoupling" in {
    testJson[ClusterCommand](
      ClusterPrepareCoupling(ClusterNodeId("A"), ClusterNodeId("B")),
      json"""{
        "TYPE": "ClusterPrepareCoupling",
        "activeId": "A",
        "passiveId": "B"
      }""")
  }

  "ClusterCouple" in {
    testJson[ClusterCommand](
      ClusterCouple(ClusterNodeId("A"), ClusterNodeId("B")),
      json"""{
        "TYPE": "ClusterCouple",
        "activeId": "A",
        "passiveId": "B"
      }""")
  }

  "ClusterRecouple" in {
    testJson[ClusterCommand](
      ClusterRecouple(ClusterNodeId("A"), ClusterNodeId("B")),
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
        ClusterNodeId("A") -> Uri("http://A"),
        ClusterNodeId("B") -> Uri("http://B")),
      activeId = ClusterNodeId("A"),
      JournalPosition(0, 1000)))),
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
