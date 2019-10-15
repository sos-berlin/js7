package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterState.{AwaitingAppointment, AwaitingFollower, Coupled, Sole}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterStateTest extends FreeSpec
{
  "JSON" - {
    "Sole" in {
      testJson[ClusterState](
        Sole(ClusterNodeId("A")),
          json"""{
            "TYPE": "Sole",
            "activeNodeId": "A"
          }""")
    }

    "AwaitingAppointment" in {
      testJson[ClusterState](
        AwaitingAppointment(ClusterNodeId("A"), Uri("http://A"), ClusterNodeId("B")),
          json"""{
            "TYPE": "AwaitingAppointment",
            "activeNodeId": "A",
            "activeUri": "http://A",
            "passiveNodeId": "B"
          }""")
    }

    "AwaitingFollower" in {
      testJson[ClusterState](
        AwaitingFollower(ClusterNodeId("A"), ClusterNodeId("B"), Uri("http://B")),
          json"""{
            "TYPE": "AwaitingFollower",
            "activeNodeId": "A",
            "passiveNodeId": "B",
            "passiveUri": "http://B"
          }""")
    }

    "Coupled" in {
      testJson[ClusterState](
        Coupled(ClusterNodeId("A"), Uri("http://A"), ClusterNodeId("B"), Uri("http://B")),
          json"""{
            "TYPE": "Coupled",
            "activeNodeId": "A",
            "activeUri": "http://A",
            "passiveNodeId": "B",
            "passiveUri": "http://B"
          }""")
    }
  }
}
