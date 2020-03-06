package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterState.{AwaitingAppointment, AwaitingFollower, Coupled, Empty, FailedOverDecoupled, OtherFailedOver, PreparedToBeCoupled, ProperlyDecoupled, Sole}
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
    "Empty" in {
      testJson[ClusterState](
        Empty,
        json"""{
          "TYPE": "Empty"
        }""")
    }

    "Sole" in {
      testJson[ClusterState](
        Sole(Uri("http://ACTIVE")),
        json"""{
          "TYPE": "Sole",
          "activeUri": "http://ACTIVE"
        }""")
    }

    "AwaitingAppointment" in {
      testJson[ClusterState](
        AwaitingAppointment(Uri("http://ACTIVE"), Uri("http://PASSIVE")),
          json"""{
            "TYPE": "AwaitingAppointment",
            "activeUri": "http://ACTIVE",
            "passiveUri": "http://PASSIVE"
          }""")
    }

    "AwaitingFollower" in {
      testJson[ClusterState](
        AwaitingFollower(Uri("http://ACTIVE"), Uri("http://PASSIVE")),
        json"""{
          "TYPE": "AwaitingFollower",
          "activeUri": "http://ACTIVE",
          "passiveUri": "http://PASSIVE"
        }""")
    }

    "PreparedToBeCoupled" in {
      testJson[ClusterState](
        PreparedToBeCoupled(Uri("http://ACTIVE"), Uri("http://PASSIVE")),
        json"""{
          "TYPE": "PreparedToBeCoupled",
          "activeUri": "http://ACTIVE",
          "passiveUri": "http://PASSIVE"
        }""")
    }

    "Coupled" in {
      testJson[ClusterState](
        Coupled(Uri("http://ACTIVE"), Uri("http://PASSIVE")),
        json"""{
          "TYPE": "Coupled",
          "activeUri": "http://ACTIVE",
          "passiveUri": "http://PASSIVE"
        }""")
    }

    "ProperlyDecoupled" in {
      testJson[ClusterState](
        ProperlyDecoupled(Uri("http://ACTIVE"), Uri("http://PASSIVE")),
        json"""{
          "TYPE": "ProperlyDecoupled",
          "activeUri": "http://ACTIVE",
          "passiveUri": "http://PASSIVE"
        }""")
    }

    "FailedOverDecoupled" in {
      testJson[ClusterState](
        FailedOverDecoupled(Uri("http://ACTIVE"), Uri("http://PASSIVE"), JournalPosition(0, 1234)),
        json"""{
          "TYPE": "FailedOverDecoupled",
          "activeUri": "http://ACTIVE",
          "passiveUri": "http://PASSIVE",
          "failedAt": {
            "fileEventId": 0,
            "position": 1234
          }
        }""")
    }

    "OtherFailedOver" in {
      testJson[ClusterState](
        OtherFailedOver(Uri("http://ACTIVE"), Uri("http://PASSIVE")),
        json"""{
          "TYPE": "OtherFailedOver",
          "activeUri": "http://ACTIVE",
          "passiveUri": "http://PASSIVE"
        }""")
    }
  }
}
