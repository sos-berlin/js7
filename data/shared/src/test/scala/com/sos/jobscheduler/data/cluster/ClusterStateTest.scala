package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterState.{Empty, IsCoupled, IsFailedOver, IsFollowerLost, IsSwitchedOver, NodesAreAppointed, PreparedToBeCoupled, Sole}
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

    //"AwaitingAppointment" in {
    //  testJson[ClusterState](
    //    AwaitingAppointment(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil),
    //      json"""{
    //        "TYPE": "AwaitingAppointment",
    //        "uris": [ "http://PRIMARY", "http://BACKUP" ]
    //      }""")
    //}

    "NodesAreAppointed" in {
      testJson[ClusterState](
        NodesAreAppointed(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil),
        json"""{
          "TYPE": "NodesAreAppointed",
          "uris": [ "http://PRIMARY", "http://BACKUP" ]
        }""")
    }

    "PreparedToBeCoupled" in {
      testJson[ClusterState](
        PreparedToBeCoupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, active = 1),
        json"""{
          "TYPE": "PreparedToBeCoupled",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 1
        }""")
    }

    "IsCoupled" in {
      testJson[ClusterState](
        IsCoupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 0),
        json"""{
          "TYPE": "IsCoupled",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 0
        }""")
    }

    "IsFollowerLost" in {
      testJson[ClusterState](
        IsFollowerLost(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 0),
        json"""{
          "TYPE": "IsFollowerLost",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 0
        }""")
    }

    "IsSwitchedOver" in {
      testJson[ClusterState](
        IsSwitchedOver(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 0),
        json"""{
          "TYPE": "IsSwitchedOver",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 0
        }""")
    }

    "IsFailedOver" in {
      testJson[ClusterState](
        IsFailedOver(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 1, JournalPosition(0, 1234)),
        json"""{
          "TYPE": "IsFailedOver",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 1,
          "failedAt": {
            "fileEventId": 0,
            "position": 1234
          }
        }""")
    }
  }
}
