package js7.data.cluster

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.ProblemException
import js7.base.test.Test
import js7.base.time.ScalaTime.*
import js7.tester.CirceJsonTester.*

final class ClusterTimingTest extends Test
{
  "JSON" in {
    testJson(ClusterTiming(1111.ms, 3333.ms),json"""
      {
        "heartbeat": 1.111,
        "heartbeatTimeout": 3.333
      }""")
  }

  "checked" in {
    assert(ClusterTiming.checked(0.s, 1.s).isLeft)
    assert(ClusterTiming.checked(1.s, 0.s).isLeft)
    assert(ClusterTiming.checked(1.s, -1.s).isLeft)
    assert(ClusterTiming.checked(-1.s, 1.s).isLeft)
    assert(ClusterTiming.checked(3.s, 1.s).isRight)
  }

  "PassiveLost is detected before FailedOver" in {
    val t = ClusterTiming(3.s, 10.s)
    assert(t.passiveLostTimeout + t.heartbeat < t.failoverTimeout)
  }

  "apply" in {
    intercept[ProblemException](ClusterTiming(0.s, 0.s))
  }

  val t = ClusterTiming(3.s, 10.s)

  "Consistency" in {
    assert(t.heartbeat < t.passiveLostTimeout)
    assert(t.passiveLostTimeout < t.clusterWatchHeartbeatValidDuration)
    assert(t.clusterWatchHeartbeatValidDuration < t.failoverTimeout)
    assert(t.clusterWatchReactionTimeout < t.failoverTimeout - t.passiveLostTimeout)
    assert(t.failoverTimeout < t.inhibitActivationDuration)
  }

  "Timings with 3s heartbeat and 10s timeout" in {
    val t = ClusterTiming(3.s, 10.s)

    assert(t.heartbeat == 3.s)
    assert(t.passiveLostTimeout == 13.s)
    assert(t.clusterWatchHeartbeatValidDuration == 16.s)
    assert(t.failoverTimeout == 19.s)
    assert(t.clusterWatchReactionTimeout == 3.s)
    assert(t.inhibitActivationDuration == 22.s)
  }
}
