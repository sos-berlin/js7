package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.*

final class SpeedTest extends OurTestSuite:

  "toString" in:
    val unit = SpeedUnit("unit", "units")
    assert(Speed(1, 1.s, unit).toString == "1 unit/s")
    assert(Speed(1, 3.s, unit).toString == "1 unit/3s")
    assert(Speed(1, 1234567.µs, unit).toString == "1 unit/1.234567s")
    assert(Speed(1, 1.ms, unit).toString == "1 unit/ms")
    assert(Speed(1, 1.minute, unit).toString == "1 unit/minute")
    assert(Speed(1, 2.minutes, unit).toString == "1 unit/2minutes")
    assert(Speed(7, 1.h, unit).toString == "7 units/h")
    assert(Speed(2, 3.days, unit).toString == "2 units/3d")
    assert(Speed(2, 3.days + 1.s, unit).toString == "2 units/3d00:00:01")
