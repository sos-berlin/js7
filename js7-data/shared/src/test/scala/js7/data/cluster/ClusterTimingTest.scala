package js7.data.cluster

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.tester.CirceJsonTester.*

final class ClusterTimingTest extends OurTestSuite:

  "JSON" in:
    testJson(ClusterTiming(1111.ms, 3333.ms, 2222.ms),json"""
      {
        "heartbeat": 1.111,
        "heartbeatTimeout": 3.333,
        "consentTimeout": 2.222
      }""")

  "JSON until 2.8.2" in:  // COMPATIBLE with v2.8.2
    // When consentTimeout is not present, we assume 6s !!!
    testJsonDecoder(ClusterTiming(1111.ms, 3333.ms, 6.s),json"""
      {
        "heartbeat": 1.111,
        "heartbeatTimeout": 3.333
      }""")

  "checked" in:
    assert(ClusterTiming(0.s, 2.s, 1.s).checked.isLeft)
    assert(ClusterTiming(1.s, 0.s, 1.s).checked.isLeft)
    assert(ClusterTiming(1.s, -1.s, 1.s).checked.isLeft)
    assert(ClusterTiming(-1.s, 2.s, 1.s).checked.isLeft)
    assert(ClusterTiming(3.s, 10.s, 10.s).checked.isLeft)
    assert(ClusterTiming(3.s, 10.s, 11.s).checked.isLeft)
    assert(ClusterTiming(3.s, 10.s, 9.s).checked.isRight)
    assert(ClusterTiming(3.s, 10.s, 6.s).checked.isRight)

  "PassiveLost is detected before FailedOver" in:
    val t = ClusterTiming(3.s, 10.s, 6.s).checked.orThrow
    assert(t.passiveLostTimeout + t.heartbeat < t.activeLostTimeout)

  "Consistency" in:
    val t = ClusterTiming(3.s, 10.s, 6.s).checked.orThrow
    assert(t.heartbeat < t.passiveLostTimeout)
    assert(t.passiveLostTimeout < t.clusterWatchHeartbeatValidDuration)
    assert(t.clusterWatchHeartbeatValidDuration < t.activeLostTimeout)
    assert(t.clusterWatchReactionTimeout < t.activeLostTimeout - t.passiveLostTimeout)
    assert(t.activeLostTimeout < t.inhibitActivationDuration)

  "Timings with 3s heartbeat and 10s timeout" in:
    val t = ClusterTiming(3.s, 10.s, 6.s).checked.orThrow

    assert(t.heartbeat == 3.s)
    assert(t.heartbeatTimeout == 10.s)

    assert(t.passiveLostTimeout == 13.s)
    assert(t.clusterWatchHeartbeatValidDuration == 16.s)
    assert(t.activeLostTimeout == 19.s)
    assert(t.inhibitActivationDuration == 22.s)

    assert(t.clusterWatchReactionTimeout == 3.s)
