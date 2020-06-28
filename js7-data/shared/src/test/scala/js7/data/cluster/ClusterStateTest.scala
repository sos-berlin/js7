package js7.data.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.cluster.ClusterState.{Coupled, CoupledActiveShutDown, Empty, FailedOver, NodesAppointed, PassiveLost, PreparedToBeCoupled, SwitchedOver}
import js7.data.event.{EventId, JournalPosition}
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterStateTest extends AnyFreeSpec
{
  "JSON" - {
    val idToUri = Map(
      NodeId("A") -> Uri("http://A"),
      NodeId("B") -> Uri("http://B"))
    "Empty" in {
      testJson[ClusterState](
        Empty,
        json"""{
          "TYPE": "Empty"
        }""")
    }

    "NodesAppointed" in {
      testJson[ClusterState](
        NodesAppointed(idToUri, NodeId("A")),
        json"""{
          "TYPE": "NodesAppointed",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "PreparedToBeCoupled" in {
      testJson[ClusterState](
        PreparedToBeCoupled(idToUri, NodeId("A")),
        json"""{
          "TYPE": "PreparedToBeCoupled",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "Coupled" in {
      testJson[ClusterState](
        Coupled(idToUri, NodeId("A")),
        json"""{
          "TYPE": "Coupled",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "CoupledActiveShutDown" in {
      testJson[ClusterState](
        CoupledActiveShutDown(idToUri, NodeId("A")),
        json"""{
          "TYPE": "CoupledActiveShutDown",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "PassiveLost" in {
      testJson[ClusterState](
        PassiveLost(idToUri, NodeId("A")),
        json"""{
          "TYPE": "PassiveLost",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "SwitchedOver" in {
      testJson[ClusterState](
        SwitchedOver(idToUri, NodeId("A")),
        json"""{
          "TYPE": "SwitchedOver",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A"
        }""")
    }

    "FailedOver" in {
      testJson[ClusterState](
        FailedOver(idToUri, NodeId("A"), JournalPosition(EventId(0), 1234)),
        json"""{
          "TYPE": "FailedOver",
          "idToUri": {
            "A": "http://A",
            "B": "http://B"
          },
          "activeId": "A",
          "failedAt": {
            "fileEventId": 0,
            "position": 1234
          }
        }""")
    }
  }
}
