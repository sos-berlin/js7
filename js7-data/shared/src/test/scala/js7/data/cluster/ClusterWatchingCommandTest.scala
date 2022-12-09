package js7.data.cluster

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchAcknowledge
import js7.tester.CirceJsonTester.testJson

final class ClusterWatchingCommandTest extends OurTestSuite
{
  "JSON" in {
    testJson[ClusterWatchingCommand](
      ClusterWatchAcknowledge(ClusterWatchMessage.RequestId(123L), Some(Problem("PROBLEM"))),
      json"""{
          "TYPE": "ClusterWatchAcknowledge",
          "requestId": 123,
          "problem": {
            "message": "PROBLEM"
          }
        }"""
    )
  }
}
