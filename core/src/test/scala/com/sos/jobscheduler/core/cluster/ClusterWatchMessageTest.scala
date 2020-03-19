package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterEvent.BecameSole
import com.sos.jobscheduler.data.cluster.ClusterState.ClusterSole
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

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
          ClusterSole(Uri("http://PRIMARY")),
          force = true),
        json"""{
          "TYPE": "ClusterWatchEvents",
          "from": "http://PRIMARY",
          "events": [
            {
              "TYPE": "Cluster.BecameSole",
              "primaryUri": "http://PRIMARY"
            }
          ],
          "clusterState": {
            "TYPE": "ClusterSole",
            "primaryUri": "http://PRIMARY"
          },
          "force": true
        }""")
    }

    "ClusterWatchHeartbeat" in {
      testJson[ClusterWatchMessage](
        ClusterWatchHeartbeat(
          Uri("http://PRIMARY"),
          ClusterSole(Uri("http://PRIMARY"))),
        json"""{
          "TYPE": "ClusterWatchHeartbeat",
          "from": "http://PRIMARY",
          "clusterState": {
            "TYPE": "ClusterSole",
            "primaryUri": "http://PRIMARY"
          }
        }""")
    }
  }
}
