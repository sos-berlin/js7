package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Throttle.TooFast
import scala.concurrent.duration.*

final class ThrottleTest extends OurTestSuite:

  private val unit = SpeedUnit("event", "events")

  "Throttle.Zero" in:
    assert(Throttle.Zero.tryRecord(0.ms, 0) == Right(Throttle.Zero))
    assert(Throttle.Zero.tryRecord(0.ms, 1) == Left(
      TooFast(
        throttle = Speed(0, 24.days * 365, unit),
        delay = 24.days * 365)))

  "Throttle.Unlimited" in:
    assert(Throttle.Unlimited.tryRecord(0.ms, 10) == Right(Throttle.Unlimited))

  "StandardThrottle" in:
    var throttle = StandardThrottle(
      Seq(
        Speed(10, 1.s, unit),
        Speed(20, 1.minute, unit)),
      unit = unit)

    throttle = throttle.tryRecord(61.s, 10).toOption.get

    assert(throttle.tryRecord(61.s + 3.ms, 1) == Left(TooFast(Speed(10, 1.s, unit), 997.ms)))
    assert(throttle.tryRecord(61.s + 333.ms, 1) == Left(TooFast(Speed(10, 1.s, unit), 667.ms)))
    assert(throttle.tryRecord(61.s + 999.ms, 1) == Left(TooFast(Speed(10, 1.s, unit), 1.ms)))

    throttle = throttle.tryRecord(63.s, 7).toOption.get
    assert(throttle.tryRecord(63.s + 7.s, 3).isRight)
    assert(throttle.tryRecord(63.s + 7.s, 4) == Left(TooFast(Speed(20, 1.minute, unit), 50.s)))
    assert(throttle.tryRecord(2.minutes - 1.ms, 4) == Left(TooFast(Speed(20, 1.minute, unit), 1.ms)))

    throttle = throttle.tryRecord(2.minutes, 4).toOption.get
