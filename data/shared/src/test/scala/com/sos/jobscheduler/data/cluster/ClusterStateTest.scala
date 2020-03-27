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
    "ClusterEmpty" in {
      testJson[ClusterState](
        ClusterEmpty,
        json"""{
          "TYPE": "ClusterEmpty"
        }""")
    }

    "ClusterNodesAppointed" in {
      testJson[ClusterState](
        ClusterNodesAppointed(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil),
        json"""{
          "TYPE": "ClusterNodesAppointed",
          "uris": [ "http://PRIMARY", "http://BACKUP" ]
        }""")
    }

    "ClusterPreparedToBeCoupled" in {
      testJson[ClusterState](
        ClusterPreparedToBeCoupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, active = 1),
        json"""{
          "TYPE": "ClusterPreparedToBeCoupled",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 1
        }""")
    }

    "ClusterCoupled" in {
      testJson[ClusterState](
        ClusterCoupled(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 0),
        json"""{
          "TYPE": "ClusterCoupled",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 0
        }""")
    }

    "ClusterPassiveLost" in {
      testJson[ClusterState](
        ClusterPassiveLost(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 0),
        json"""{
          "TYPE": "ClusterPassiveLost",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 0
        }""")
    }

    "ClusterSwitchedOver" in {
      testJson[ClusterState](
        ClusterSwitchedOver(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 0),
        json"""{
          "TYPE": "ClusterSwitchedOver",
          "uris": [ "http://PRIMARY", "http://BACKUP" ],
          "active": 0
        }""")
    }

    "ClusterFailedOver" in {
      testJson[ClusterState](
        ClusterFailedOver(Uri("http://PRIMARY") :: Uri("http://BACKUP") :: Nil, 1, JournalPosition(0, 1234)),
        json"""{
          "TYPE": "ClusterFailedOver",
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
