package js7.data.cluster

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.tester.CirceJsonTester.*

final class ClusterTimingTest extends OurTestSuite:

  "JSON" in:
    testJson(ClusterTiming(1111.ms, 3333.ms),json"""
      {
        "heartbeat": 1.111,
        "heartbeatTimeout": 3.333
      }""")

  "checked" in:
    assert(ClusterTiming(0.s, 1.s).checked.isLeft)
    assert(ClusterTiming(1.s, 0.s).checked.isLeft)
    assert(ClusterTiming(1.s, -1.s).checked.isLeft)
    assert(ClusterTiming(-1.s, 1.s).checked.isLeft)
    assert(ClusterTiming(3.s, 1.s).checked.isRight)

  "PassiveLost is detected before FailedOver" in:
    val t = ClusterTiming(3.s, 10.s)
    assert(t.passiveLostTimeout + t.heartbeat < t.activeLostTimeout)

  val t = ClusterTiming(3.s, 10.s)

  "Consistency" in:
    assert(t.heartbeat < t.passiveLostTimeout)
    assert(t.passiveLostTimeout < t.clusterWatchHeartbeatValidDuration)
    assert(t.clusterWatchHeartbeatValidDuration < t.activeLostTimeout)
    assert(t.clusterWatchReactionTimeout < t.activeLostTimeout - t.passiveLostTimeout)
    assert(t.activeLostTimeout < t.inhibitActivationDuration)

  "Timings with 3s heartbeat and 10s timeout" in:
    val t = ClusterTiming(3.s, 10.s)

    assert(t.heartbeat == 3.s)
    assert(t.heartbeatTimeout == 10.s)

    assert(t.passiveLostTimeout == 13.s)
    assert(t.clusterWatchHeartbeatValidDuration == 16.s)
    assert(t.activeLostTimeout == 19.s)
    assert(t.inhibitActivationDuration == 22.s)

    assert(t.clusterWatchReactionTimeout == 3.s)
