package js7.data.cluster

import js7.base.circeutils.CirceUtils.*
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterNodeStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[ClusterNodeState](
      ClusterNodeState(NodeId("MY-NODE"), true, ClusterState.Empty),
      json"""{
        "nodeId": "MY-NODE",
        "isBackup": true,
        "clusterState": {
          "TYPE": "Empty"
        }
      }""")
  }
}
