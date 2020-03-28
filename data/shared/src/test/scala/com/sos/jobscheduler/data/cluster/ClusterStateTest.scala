package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterState.{Coupled, Empty, FailedOver, NodesAppointed, PassiveLost, PreparedToBeCoupled, SwitchedOver}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.JournalPosition
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterStateTest extends FreeSpec
{
  "JSON" - {
    val idToUri = Map(
      ClusterNodeId("A") -> Uri("http://A"),
      ClusterNodeId("B") -> Uri("http://B"))
    "Empty" in {
      testJson[ClusterState](
        Empty,
        json"""{
          "TYPE": "Empty"
        }""")
    }

    "NodesAppointed" in {
      testJson[ClusterState](
        NodesAppointed(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "NodesAppointed",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "PreparedToBeCoupled" in {
      testJson[ClusterState](
        PreparedToBeCoupled(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "PreparedToBeCoupled",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "Coupled" in {
      testJson[ClusterState](
        Coupled(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "Coupled",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "PassiveLost" in {
      testJson[ClusterState](
        PassiveLost(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "PassiveLost",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "SwitchedOver" in {
      testJson[ClusterState](
        SwitchedOver(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "SwitchedOver",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "FailedOver" in {
      testJson[ClusterState](
        FailedOver(idToUri, ClusterNodeId("A"), JournalPosition(0, 1234)),
        json"""{
          "TYPE": "FailedOver",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A",
          "failedAt": {
            "fileEventId": 0,
            "position": 1234
          }
        }""")
    }
  }
}
