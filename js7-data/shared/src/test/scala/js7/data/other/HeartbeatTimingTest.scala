package js7.data.other

import io.circe.DecodingFailure
import js7.base.circeutils.CirceUtils._
import js7.base.time.ScalaTime._
import js7.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

final class HeartbeatTimingTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(HeartbeatTiming(1111.ms, 3333.ms),json"""
      {
        "heartbeat": 1.111,
        "heartbeatTimeout": 3.333
      }""")

    val invalidJson = json"""{
      "heartbeat": 0,
      "heartbeatTimeout": 3
    }"""
    assert(invalidJson.as[HeartbeatTiming] == Left(DecodingFailure(
      "Invalid heartbeat timing values", Nil)))
  }

  "checked" in {
    assert(HeartbeatTiming.checked(0.s, 1.s).isLeft)
    assert(HeartbeatTiming.checked(1.s, 0.s).isLeft)
    assert(HeartbeatTiming.checked(1.s, -1.s).isLeft)
    assert(HeartbeatTiming.checked(-1.s, 1.s).isLeft)
    assert(HeartbeatTiming.checked(3.s, 1.s).isRight)
  }

  "check" in {
    assert(HeartbeatTiming(1.s, 0.s).checked.isLeft)
    assert(HeartbeatTiming(1.s, 1.s).checked.isRight)
  }
}
