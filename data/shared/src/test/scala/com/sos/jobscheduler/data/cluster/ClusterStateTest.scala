package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterState.{ClusterCoupled, ClusterEmpty, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterPreparedToBeCoupled, ClusterSwitchedOver}
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
    "ClusterEmpty" in {
      testJson[ClusterState](
        ClusterEmpty,
        json"""{
          "TYPE": "ClusterEmpty"
        }""")
    }

    "ClusterNodesAppointed" in {
      testJson[ClusterState](
        ClusterNodesAppointed(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "ClusterNodesAppointed",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "ClusterPreparedToBeCoupled" in {
      testJson[ClusterState](
        ClusterPreparedToBeCoupled(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "ClusterPreparedToBeCoupled",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "ClusterCoupled" in {
      testJson[ClusterState](
        ClusterCoupled(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "ClusterCoupled",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "ClusterPassiveLost" in {
      testJson[ClusterState](
        ClusterPassiveLost(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "ClusterPassiveLost",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "ClusterSwitchedOver" in {
      testJson[ClusterState](
        ClusterSwitchedOver(idToUri, ClusterNodeId("A")),
        json"""{
          "TYPE": "ClusterSwitchedOver",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "ClusterFailedOver" in {
      testJson[ClusterState](
        ClusterFailedOver(idToUri, ClusterNodeId("A"), JournalPosition(0, 1234)),
        json"""{
          "TYPE": "ClusterFailedOver",
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
