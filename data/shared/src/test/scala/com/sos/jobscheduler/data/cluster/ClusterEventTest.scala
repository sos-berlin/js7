package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.cluster.ClusterEvent.{Coupled, CouplingPrepared, FailedOver, NodesAppointed, PassiveLost, SwitchedOver}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.JournalPosition
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterEventTest extends FreeSpec
{
  private val Id = ClusterNodeId

  "NodesAppointed" in {
    testJson[ClusterEvent](NodesAppointed(
      Map(
        Id("A") -> Uri("http://PRIMARY"),
        Id("B") -> Uri("http://BACKUP")),
      Id("A")),
      json"""{
        "TYPE": "Cluster.NodesAppointed",
        "idToUri": {
          "A":  "http://PRIMARY",
          "B": "http://BACKUP"
        },
        "activeId": "A"
      }""")
  }

  "CouplingPrepared" in {
    testJson[ClusterEvent](CouplingPrepared(Id("A")),
      json"""{
        "TYPE": "Cluster.CouplingPrepared",
        "activeId": "A"
      }""")
  }

  "Coupled" in {
    testJson[ClusterEvent](Coupled(ClusterNodeId("A")),
      json"""{
        "TYPE": "Cluster.Coupled",
        "activeId": "A"
      }""")
  }

  "SwitchedOver" in {
    testJson[ClusterEvent](SwitchedOver(Id("B")),
      json"""{
        "TYPE": "Cluster.SwitchedOver",
        "toId": "B"
      }""")
  }

  "FailedOver" in {
    testJson[ClusterEvent](FailedOver(Id("A"), Id("B"), JournalPosition(0, 1234)),
      json"""{
        "TYPE": "Cluster.FailedOver",
        "failedActiveId": "A",
        "activatedId": "B",
        "failedAt": {
          "fileEventId": 0,
          "position": 1234
        }
      }""")
  }

  "PassiveLost" in {
    testJson[ClusterEvent](PassiveLost(Id("B")),
      json"""{
        "TYPE": "Cluster.PassiveLost",
        "id": "B"
      }""")
  }
}
