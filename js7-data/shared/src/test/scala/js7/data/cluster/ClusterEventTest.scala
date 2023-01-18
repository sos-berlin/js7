package js7.data.cluster

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeRestarted, ClusterActiveNodeShutDown, ClusterCoupled, ClusterCouplingPrepared, ClusterFailedOver, ClusterNodesAppointed, ClusterPassiveLost, ClusterSettingUpdated, ClusterSwitchedOver, ClusterWatchRegistered}
import js7.data.event.{EventId, JournalPosition}
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class ClusterEventTest extends OurTestSuite
{
  private val Id = NodeId

  "ClusterNodesAppointed" in {
    testJson[ClusterEvent](ClusterNodesAppointed(
      ClusterSetting(
        Map(
          NodeId("PRIMARY") -> Uri("https://PRIMARY"),
          NodeId("BACKUP") -> Uri("https://BACKUP")),
        NodeId("PRIMARY"),
        ClusterTiming(10.s, 20.s),
        Some(ClusterWatchId("CLUSTER-WATCH")),
        Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))))),
      json"""{
        "TYPE": "ClusterNodesAppointed",
        "setting": {
          "idToUri": {
            "PRIMARY": "https://PRIMARY",
            "BACKUP": "https://BACKUP"
          },
          "activeId": "PRIMARY",
          "timing": {
            "heartbeat": 10,
            "heartbeatTimeout": 20
          },
          "clusterWatchId": "CLUSTER-WATCH",
          "clusterWatches": [ { "uri": "https://CLUSTER-WATCH" } ]
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

  "ClusterSettingUpdated" - {
    "passiveUri only" in {
      testJson[ClusterEvent](ClusterSettingUpdated(passiveUri = Some(Uri("https://PASSIVE"))),
        json"""{
          "TYPE": "ClusterSettingUpdated",
          "passiveUri": "https://PASSIVE"
        }""")
    }

    "clusterWatch only" in {
      testJson[ClusterEvent](ClusterSettingUpdated(clusterWatches = Some(Seq(ClusterSetting.Watch(Uri("https://A"))))),
        json"""{
          "TYPE": "ClusterSettingUpdated",
          "clusterWatches": [
            {
              "uri": "https://A"
            }
          ]
        }""")
    }

    "complete" in {
      testJson[ClusterEvent](ClusterSettingUpdated(Some(Uri("https://PASSIVE")), Some(Seq(ClusterSetting.Watch(Uri("https://A"))))),
        json"""{
          "TYPE": "ClusterSettingUpdated",
          "passiveUri": "https://PASSIVE",
          "clusterWatches": [
            {
              "uri": "https://A"
            }
          ]
        }""")
    }
  }

  "ClusterWatchRegistered" in {
    testJson[ClusterEvent](ClusterWatchRegistered(ClusterWatchId("CLUSTER-WATCH")),
      json"""{
      "TYPE": "ClusterWatchRegistered",
      "clusterWatchId": "CLUSTER-WATCH"
    }""")
  }
}
