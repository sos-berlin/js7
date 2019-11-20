package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{BackupNodeAppointed, BecameSole, ClusterCoupled, FollowingStarted, SwitchedOver}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterEventTest extends FreeSpec
{
  "BecameSole" in {
    testJson[ClusterEvent](BecameSole(Uri("http://ACTIVE")),
      json"""{
        "TYPE": "BecameSole",
        "activeUri": "http://ACTIVE"
      }""")
  }

  "BackupNodeAppointed" in {
    testJson[ClusterEvent](BackupNodeAppointed(Uri("http://BACKUP")),
      json"""{
        "TYPE": "BackupNodeAppointed",
        "uri": "http://BACKUP"
      }""")
  }

  "FollowingStarted" in {
    testJson[ClusterEvent](FollowingStarted(Uri("http://FOLLOWER")),
      json"""{
        "TYPE": "FollowingStarted",
        "followingUri": "http://FOLLOWER"
      }""")
  }

  "ClusterCoupled" in {
    testJson[ClusterEvent](ClusterCoupled,
      json"""{
        "TYPE": "ClusterCoupled"
      }""")
  }

  "SwitchedOver" in {
    testJson[ClusterEvent](SwitchedOver(Uri("http://NODE")),
      json"""{
        "TYPE": "SwitchedOver",
        "uri": "http://NODE"
      }""")
  }
}
