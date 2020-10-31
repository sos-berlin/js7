package js7.data.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.data.cluster.ClusterState.{ActiveShutDown, Coupled, Empty, FailedOver, NodesAppointed, PassiveLost, PreparedToBeCoupled, SwitchedOver}
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
      NodeId("A"),
      Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
      ClusterTiming(10.s, 20.s))

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
            "activeId": "A",
            "clusterWatches": [
              {
                "uri": "https://CLUSTER-WATCH"
              }
            ],
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            }
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
            "activeId": "A",
            "clusterWatches": [
              {
                "uri": "https://CLUSTER-WATCH"
              }
            ],
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            }
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
            "activeId": "A",
            "clusterWatches": [
              {
                "uri": "https://CLUSTER-WATCH"
              }
            ],
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            }
          }
        }""")
    }

    "ActiveShutDown" in {
      testJson[ClusterState](
        ActiveShutDown(setting),
        json"""{
          "TYPE": "ActiveShutDown",
          "setting": {
            "idToUri": {
              "A": "http://A",
              "B": "http://B"
            },
            "activeId": "A",
            "clusterWatches": [
              {
                "uri": "https://CLUSTER-WATCH"
              }
            ],
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            }
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
            "activeId": "A",
            "clusterWatches": [
              {
                "uri": "https://CLUSTER-WATCH"
              }
            ],
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            }
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
            "activeId": "A",
            "clusterWatches": [
              {
                "uri": "https://CLUSTER-WATCH"
              }
            ],
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            }
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
            "activeId": "A",
            "clusterWatches": [
              {
                "uri": "https://CLUSTER-WATCH"
              }
            ],
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            }
          },
          "failedAt": {
            "fileEventId": 0,
            "position": 1234
          }
        }""")
    }
  }
}
