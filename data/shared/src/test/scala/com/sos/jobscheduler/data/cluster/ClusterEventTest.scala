package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.cluster.ClusterEvent.{ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import com.sos.jobscheduler.data.event.JournalPosition
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterEventTest extends FreeSpec
{
  private val Id = ClusterNodeId

  "ClusterNodesAppointed" in {
    testJson[ClusterEvent](ClusterNodesAppointed(
      Map(
        Id("A") -> Uri("http://PRIMARY"),
        Id("B") -> Uri("http://BACKUP")),
      Id("A")),
      json"""{
        "TYPE": "ClusterNodesAppointed",
        "idToUri": {
          "A":  "http://PRIMARY",
          "B": "http://BACKUP"
        },
        "activeId": "A"
      }""")
  }

  "ClusterCouplingPrepared" in {
    testJson[ClusterEvent](ClusterCouplingPrepared(Id("A")),
      json"""{
        "TYPE": "ClusterCouplingPrepared",
        "activeId": "A"
      }""")
  }

  "Coupled" in {
    testJson[ClusterEvent](ClusterCoupled(ClusterNodeId("A")),
      json"""{
        "TYPE": "ClusterCoupled",
        "activeId": "A"
      }""")
  }

  "SwitchedOver" in {
    testJson[ClusterEvent](ClusterSwitchedOver(Id("B")),
      json"""{
        "TYPE": "ClusterSwitchedOver",
        "toId": "B"
      }""")
  }

  "FailedOver" in {
    testJson[ClusterEvent](ClusterFailedOver(Id("A"), Id("B"), JournalPosition(0, 1234)),
      json"""{
        "TYPE": "ClusterFailedOver",
        "failedActiveId": "A",
        "activatedId": "B",
        "failedAt": {
          "fileEventId": 0,
          "position": 1234
        }
      }""")
  }

  "PassiveLost" in {
    testJson[ClusterEvent](ClusterPassiveLost(Id("B")),
      json"""{
        "TYPE": "ClusterPassiveLost",
        "id": "B"
      }""")
  }
}
