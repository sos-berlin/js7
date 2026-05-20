package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.SpeedLimiter.TooFast
import scala.concurrent.duration.*

final class SpeedLimiterTest extends OurTestSuite:

  "SpeedLimiter.Zero" in:
    assert(SpeedLimiter.Zero.tryRecord(0.ms, 0) == Right(SpeedLimiter.Zero))
    assert(SpeedLimiter.Zero.tryRecord(0.ms, 1) == Left(SpeedLimiter.Zero.tooFast))

  "SpeedLimiter.Unlimited" in:
    assert(SpeedLimiter.Unlimited.tryRecord(0.ms, 10) == Right(SpeedLimiter.Unlimited))

  "StandardSpeedLimiter" in:
    var speedLimiter = StandardSpeedLimiter(Seq(
      Speed(10, 1.s),
      Speed(20, 1.minute)))

    speedLimiter = speedLimiter.tryRecord(0.ms, 10).toOption.get
    assert(speedLimiter.tryRecord(3.ms, 1) == Left(TooFast(Speed(10, 1.s), 997.ms)))
    assert(speedLimiter.tryRecord(999.ms, 1) == Left(TooFast(Speed(10, 1.s), 1.ms)))

    speedLimiter = speedLimiter.tryRecord(2.s, 7).toOption.get
    assert(speedLimiter.tryRecord(7.s, 3).isRight)
    assert(speedLimiter.tryRecord(7.s, 4) == Left(TooFast(Speed(20, 1.minute), 53.s)))
    assert(speedLimiter.tryRecord(59.s, 4) == Left(TooFast(Speed(20, 1.minute), 1.s)))

    speedLimiter = speedLimiter.tryRecord(1.minute, 4).toOption.get
