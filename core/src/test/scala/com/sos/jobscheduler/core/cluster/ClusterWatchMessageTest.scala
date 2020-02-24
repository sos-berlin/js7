package com.sos.jobscheduler.core.cluster

import org.scalatest.FreeSpec
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterEvent.BecameSole
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.common.Uri

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchMessageTest extends FreeSpec
{
  "JSON" - {
    "ClusterWatchEvents" in {
      testJson[ClusterWatchMessage](
        ClusterWatchEvents(
          Uri("http://PRIMARY"),
          BecameSole(Uri("http://PRIMARY")) :: Nil,
          ClusterState.Sole(Uri("http://PRIMARY"))),
        json"""{
          "TYPE": "ClusterWatchEvents",
          "from": "http://PRIMARY",
          "events": [
            {
              "TYPE": "BecameSole",
              "activeUri": "http://PRIMARY"
            }
          ],
          "clusterState": {
            "TYPE": "Sole",
            "activeUri": "http://PRIMARY"
          }
        }""")
    }

    "ClusterWatchHeartbeat" in {
      pending
    }
  }
}
