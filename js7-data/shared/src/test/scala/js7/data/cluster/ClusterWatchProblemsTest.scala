package js7.data.cluster

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.cluster.ClusterEvent.ClusterPassiveLost
import js7.data.cluster.ClusterWatchProblems.ClusterNodeLossNotConfirmedProblem
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson

final class ClusterWatchProblemsTest extends OurTestSuite {

  "ClusterNodeLossNotConfirmedProblem" in {
    testJson(
      ClusterNodeLossNotConfirmedProblem(NodeId("Primary"), ClusterPassiveLost(NodeId("Backup"))),
      json"""{
          "TYPE": "ClusterNodeLossNotConfirmedProblem",
          "event": {
            "TYPE": "ClusterPassiveLost",
            "id": "Backup"
          },
          "fromNodeId": "Primary"
        }""")
  }
}
