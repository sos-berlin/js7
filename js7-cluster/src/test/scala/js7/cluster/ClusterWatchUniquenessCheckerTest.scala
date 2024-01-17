package js7.cluster

import js7.base.test.OurTestSuite
import js7.data.cluster.{ClusterWatchId, ClusterWatchRunId}

final class ClusterWatchUniquenessCheckerTest extends OurTestSuite:
  "check" in:
    val checker = new ClusterWatchUniquenessChecker(memorySize = 3)

    assert(checker.test(ClusterWatchId("A"), clusterWatchRunId('1')).isRight)
    assert(checker.test(ClusterWatchId("A"), clusterWatchRunId('1')).isRight)
    assert(checker.test(ClusterWatchId("A"), clusterWatchRunId('2')).isRight)

    assert(checker.test(ClusterWatchId("B"), clusterWatchRunId('1')).isLeft)
    assert(checker.test(ClusterWatchId("B"), clusterWatchRunId('2')).isLeft)
    assert(checker.test(ClusterWatchId("B"), clusterWatchRunId('3')).isRight)
    assert(checker.test(ClusterWatchId("B"), clusterWatchRunId('4')).isRight)

    // Due to low memorySize=3, the first ClusterWatchRunId is forgotten
    assert(checker.test(ClusterWatchId("B"), clusterWatchRunId('1')).isRight)

    assert(checker.test(ClusterWatchId("B"), clusterWatchRunId('5')).isRight)

    // But the ClusterWatchId('2") used for A, is checked despite memorySize
    assert(checker.test(ClusterWatchId("B"), clusterWatchRunId('2')).isLeft)

  private def clusterWatchRunId(char: Char) =
    ClusterWatchRunId(char.toString * 22)
