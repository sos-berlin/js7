package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.SpeedLimiter.TooFast
import scala.concurrent.duration.*

final class SpeedLimiterTest extends OurTestSuite:

  private val unit = SpeedUnit("event", "events")

  "SpeedLimiter.Zero" in:
    assert(SpeedLimiter.Zero.tryRecord(0.ms, 0) == Right(SpeedLimiter.Zero))
    assert(SpeedLimiter.Zero.tryRecord(0.ms, 1) == Left(
      TooFast(
        speedLimit = Speed(0, 24.days * 365, unit),
        delay = 24.days * 365)))

  "SpeedLimiter.Unlimited" in:
    assert(SpeedLimiter.Unlimited.tryRecord(0.ms, 10) == Right(SpeedLimiter.Unlimited))

  "StandardSpeedLimiter" in:
    var speedLimiter = StandardSpeedLimiter(
      Seq(
        Speed(10, 1.s, unit),
        Speed(20, 1.minute, unit)),
      unit = unit)

    speedLimiter = speedLimiter.tryRecord(61.s, 10).toOption.get

    assert(speedLimiter.tryRecord(61.s + 3.ms, 1) == Left(TooFast(Speed(10, 1.s, unit), 997.ms)))
    assert(speedLimiter.tryRecord(61.s + 333.ms, 1) == Left(TooFast(Speed(10, 1.s, unit), 667.ms)))
    assert(speedLimiter.tryRecord(61.s + 999.ms, 1) == Left(TooFast(Speed(10, 1.s, unit), 1.ms)))

    speedLimiter = speedLimiter.tryRecord(63.s, 7).toOption.get
    assert(speedLimiter.tryRecord(63.s + 7.s, 3).isRight)
    assert(speedLimiter.tryRecord(63.s + 7.s, 4) == Left(TooFast(Speed(20, 1.minute, unit), 50.s)))
    assert(speedLimiter.tryRecord(2.minutes - 1.ms, 4) == Left(TooFast(Speed(20, 1.minute, unit), 1.ms)))

    speedLimiter = speedLimiter.tryRecord(2.minutes, 4).toOption.get
