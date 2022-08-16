package js7.data.cluster

import js7.base.circeutils.CirceUtils.*
import js7.base.test.Test
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class ClusterNodeStateTest extends Test
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
