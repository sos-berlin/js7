package js7.data.cluster

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.utils.Base64UUID
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.tester.CirceJsonTester.testJson

final class ClusterWatchingCommandTest extends OurTestSuite:
  "JSON" in:
    testJson[ClusterWatchingCommand](
      ClusterWatchConfirm(
        ClusterWatchRequest.RequestId(123L),
        ClusterWatchId("WATCH"),
        ClusterWatchRunId(Base64UUID.zero.string),
        manualConfirmer = Some(Confirmer("SOMEONE")),
        Some(Problem("PROBLEM"))),
      json"""{
          "TYPE": "ClusterWatchConfirm",
          "clusterWatchId": "WATCH",
          "clusterWatchRunId": "AAAAAAAAAAAAAAAAAAAAAA",
          "requestId": 123,
          "manualConfirmer": "SOMEONE",
          "problem": {
            "message": "PROBLEM"
          }
        }"""
    )
