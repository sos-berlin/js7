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

  "ClusterCoupled" in {
    testJson[ClusterEvent](ClusterCoupled(NodeId("A")),
      json"""{
        "TYPE": "ClusterCoupled",
        "activeId": "A"
      }""")
  }

  "ClusterSwitchedOver" in {
    testJson[ClusterEvent](ClusterSwitchedOver(Id("B")),
      json"""{
        "TYPE": "ClusterSwitchedOver",
        "toId": "B"
      }""")
  }

  "ClusterFailedOver" in {
    testJson[ClusterEvent](ClusterFailedOver(Id("A"), Id("B"), JournalPosition(EventId(0), 1234)),
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

  "ClusterPassiveLost" in {
    testJson[ClusterEvent](ClusterPassiveLost(Id("B")),
      json"""{
        "TYPE": "ClusterPassiveLost",
        "id": "B"
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
