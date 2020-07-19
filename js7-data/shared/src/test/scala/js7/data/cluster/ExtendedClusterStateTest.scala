package js7.data.cluster

import js7.base.circeutils.CirceUtils._
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExtendedClusterStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[ExtendedClusterState](
      ExtendedClusterState(NodeId("MY-NODE"), ClusterState.Empty),
      json"""{
        "nodeId": "MY-NODE",
        "clusterState": {
          "TYPE": "Empty"
        }
      }""")
  }
}
