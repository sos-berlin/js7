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
        Sole(Uri("http://PRIMARY")),
        json"""{
          "TYPE": "Sole",
          "primaryUri": "http://PRIMARY"
        }""")
    }

    "AwaitingAppointment" in {
      testJson[ClusterState](
        AwaitingAppointment(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil),
          json"""{
            "TYPE": "AwaitingAppointment",
            "uris": [ "http://PRIMARY", "http://BACKUP" ]
          }""")
    }

    "AwaitingFollower" in {
      testJson[ClusterState](
        AwaitingFollower(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil),
        json"""{
          "TYPE": "AwaitingFollower",
          "uris": [ "http://PRIMARY", "http://BACKUP" ]
        }""")
    }

    "PreparedToBeCoupled" in {
      testJson[ClusterState](
        PreparedToBeCoupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil),
        json"""{
          "TYPE": "PreparedToBeCoupled",
          "uris": [ "http://PRIMARY", "http://BACKUP" ]
        }""")
    }

    "Coupled" in {
      testJson[ClusterState](
        Coupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 0),
        json"""{
          "TYPE": "Coupled",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 0
        }""")
    }

    "ProperlyDecoupled" in {
      testJson[ClusterState](
        ProperlyDecoupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 0),
        json"""{
          "TYPE": "ProperlyDecoupled",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 0
        }""")
    }

    "FailedOverDecoupled" in {
      testJson[ClusterState](
        FailedOverDecoupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 1, JournalPosition(0, 1234)),
        json"""{
          "TYPE": "FailedOverDecoupled",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 1,
          "failedAt": {
            "fileEventId": 0,
            "position": 1234
          }
        }""")
    }

    "OtherFailedOver" in {
      testJson[ClusterState](
        OtherFailedOver(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 1),
        json"""{
          "TYPE": "OtherFailedOver",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 1
        }""")
    }
  }
}
