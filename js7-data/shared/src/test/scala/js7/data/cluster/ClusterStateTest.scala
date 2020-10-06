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
    val setting = ClusterSetting(
      Map(
        NodeId("A") -> Uri("http://A"),
        NodeId("B") -> Uri("http://B")),
      NodeId("A"))

    "Empty" in {
      testJson[ClusterState](
        Empty,
        json"""{
          "TYPE": "Empty"
        }""")
    }

    "NodesAppointed" in {
      testJson[ClusterState](
        NodesAppointed(setting),
        json"""{
          "TYPE": "NodesAppointed",
          "setting": {
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          }
        }""")
    }

    "PreparedToBeCoupled" in {
      testJson[ClusterState](
        PreparedToBeCoupled(setting),
        json"""{
          "TYPE": "PreparedToBeCoupled",
          "setting": {
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          }
        }""")
    }

    "Coupled" in {
      testJson[ClusterState](
        Coupled(setting),
        json"""{
          "TYPE": "Coupled",
          "setting": {
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          }
        }""")
    }

    "CoupledActiveShutDown" in {
      testJson[ClusterState](
        CoupledActiveShutDown(setting),
        json"""{
          "TYPE": "CoupledActiveShutDown",
          "setting": {
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          }
        }""")
    }

    "PassiveLost" in {
      testJson[ClusterState](
        PassiveLost(setting),
        json"""{
          "TYPE": "PassiveLost",
          "setting": {
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          }
        }""")
    }

    "SwitchedOver" in {
      testJson[ClusterState](
        SwitchedOver(setting),
        json"""{
          "TYPE": "SwitchedOver",
          "setting": {
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          }
        }""")
    }

    "FailedOver" in {
      testJson[ClusterState](
        FailedOver(setting, JournalPosition(EventId(0), 1234)),
        json"""{
          "TYPE": "FailedOver",
          "setting": {
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A"
          },
          "failedAt": {
            "fileEventId": 0,
            "position": 1234
          }
        }""")
    }
  }
}
