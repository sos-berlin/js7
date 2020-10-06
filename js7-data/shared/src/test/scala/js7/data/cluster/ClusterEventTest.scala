package js7.data.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.event.{EventId, JournalPosition}
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterEventTest extends AnyFreeSpec
{
  private val Id = NodeId

  "ClusterNodesAppointed" in {
    testJson[ClusterEvent](ClusterNodesAppointed(
      ClusterSetting(
        Map(
          NodeId("PRIMARY") -> Uri("https://PRIMARY"),
          NodeId("BACKUP") -> Uri("https://BACKUP")),
        NodeId("PRIMARY"))),
      json"""{
        "TYPE": "ClusterNodesAppointed",
        "setting": {
          "idToUri": {
            "PRIMARY": "https://PRIMARY",
            "BACKUP": "https://BACKUP"
          },
          "activeId": "PRIMARY"
        }
      }""")
  }

  "ClusterCouplingPrepared" in {
    testJson[ClusterEvent](ClusterCouplingPrepared(Id("PRIMARY")),
      json"""{
        "TYPE": "ClusterCouplingPrepared",
        "activeId": "PRIMARY"
      }""")
  }

  "ClusterCoupled" in {
    testJson[ClusterEvent](ClusterCoupled(NodeId("PRIMARY")),
      json"""{
        "TYPE": "ClusterCoupled",
        "activeId": "PRIMARY"
      }""")
  }

  "ClusterSwitchedOver" in {
    testJson[ClusterEvent](ClusterSwitchedOver(Id("BACKUP")),
      json"""{
        "TYPE": "ClusterSwitchedOver",
        "activatedId": "BACKUP"
      }""")
  }

  "ClusterFailedOver" in {
    testJson[ClusterEvent](ClusterFailedOver(Id("PRIMARY"), Id("BACKUP"), JournalPosition(EventId(0), 1234)),
      json"""{
        "TYPE": "ClusterFailedOver",
        "failedActiveId": "PRIMARY",
        "activatedId": "BACKUP",
        "failedAt": {
          "fileEventId": 0,
          "position": 1234
        }
      }""")
  }

  "ClusterPassiveLost" in {
    testJson[ClusterEvent](ClusterPassiveLost(Id("BACKUP")),
      json"""{
        "TYPE": "ClusterPassiveLost",
        "id": "BACKUP"
      }""")
  }

  "ClusterActiveNodeShutDown" in {
    testJson[ClusterEvent](ClusterActiveNodeShutDown,
      json"""{
        "TYPE": "ClusterActiveNodeShutDown"
      }""")
  }

  "ClusterActiveNodeRestarted" in {
    testJson[ClusterEvent](ClusterActiveNodeRestarted,
      json"""{
        "TYPE": "ClusterActiveNodeRestarted"
      }""")
  }
}
