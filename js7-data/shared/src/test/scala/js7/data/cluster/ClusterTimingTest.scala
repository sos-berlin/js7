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

  "apply" in {
    intercept[ProblemException](ClusterTiming(0.s, 0.s))
  }
}
