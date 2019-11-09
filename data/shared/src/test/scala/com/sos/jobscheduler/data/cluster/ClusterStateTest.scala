package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterState.{AwaitingAppointment, AwaitingFollower, Coupled, Empty, PreparedToBeCoupled, Sole}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterStateTest extends FreeSpec
{
  "JSON" - {
    "Empty" in {
      testJson[ClusterState](
        Empty,
        json"""{
          "TYPE": "Empty"
        }""")
    }

    "Sole" in {
      testJson[ClusterState](
        Sole(ClusterNodeId("NODE-ID")),
        json"""{
          "TYPE": "Sole",
          "activeNodeId": "NODE-ID"
        }""")
    }

    "AwaitingAppointment" in {
      testJson[ClusterState](
        AwaitingAppointment(ClusterNodeId("ACTIVE"), Uri("http://A"), ClusterNodeId("PASSIVE")),
          json"""{
            "TYPE": "AwaitingAppointment",
            "activeNodeId": "ACTIVE",
            "activeUri": "http://A",
            "passiveNodeId": "PASSIVE"
          }""")
    }

    "AwaitingFollower" in {
      testJson[ClusterState](
        AwaitingFollower(ClusterNodeId("ACTIVE"), ClusterNodeId("PASSIVE"), Uri("http://B")),
        json"""{
          "TYPE": "AwaitingFollower",
          "activeNodeId": "ACTIVE",
          "passiveNodeId": "PASSIVE",
          "passiveUri": "http://B"
        }""")
    }

    "PreparedToBeCoupled" in {
      testJson[ClusterState](
        PreparedToBeCoupled(ClusterNodeId("ACTIVE"), Uri("http://A"), ClusterNodeId("PASSIVE"), Uri("http://B")),
        json"""{
          "TYPE": "PreparedToBeCoupled",
          "activeNodeId": "ACTIVE",
          "activeUri": "http://A",
          "passiveNodeId": "PASSIVE",
          "passiveUri": "http://B"
        }""")
    }

    "Coupled" in {
      testJson[ClusterState](
        Coupled(ClusterNodeId("ACTIVE"), Uri("http://A"), ClusterNodeId("PASSIVE"), Uri("http://B")),
        json"""{
          "TYPE": "Coupled",
          "activeNodeId": "ACTIVE",
          "activeUri": "http://A",
          "passiveNodeId": "PASSIVE",
          "passiveUri": "http://B"
        }""")
    }
  }
}
