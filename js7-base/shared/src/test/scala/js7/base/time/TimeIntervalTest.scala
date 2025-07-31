package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import scala.concurrent.duration.*

final class TimeIntervalTest extends OurTestSuite:
  private val timeInterval = TimeInterval(ts"2021-08-23T00:00:00Z", 1.h)

  "endsBefore" in:
    assert(!timeInterval.endsBefore(timeInterval.start - 1.ms))
    assert(!timeInterval.endsBefore(timeInterval.start))
    assert(!timeInterval.endsBefore(timeInterval.start + timeInterval.duration - 1.ms))
    assert(timeInterval.endsBefore(timeInterval.start + timeInterval.duration))
    assert(timeInterval.endsBefore(timeInterval.start + timeInterval.duration + 1.ms))

  "contains" in:
    assert(!timeInterval.contains(timeInterval.start - 1.ms))
    assert(timeInterval.contains(timeInterval.start))
    assert(timeInterval.contains(timeInterval.start + timeInterval.duration - 1.ms))
    assert(!timeInterval.contains(timeInterval.start + timeInterval.duration))

  "Never" in:
    assert(!TimeInterval.Never.contains(Timestamp.now))
    assert(!TimeInterval.Never.contains(Timestamp.now - 100*365*24.h))
    assert(!TimeInterval.Never.contains(Timestamp.now + 100*365*24.h))
    assert(TimeInterval.Never.endsBefore(Timestamp.Epoch))
    assert(TimeInterval.Never.endsBefore(Timestamp.now + 100*365*24.h))

  "Always" in:
    assert(TimeInterval.Always.contains(Timestamp.now))
    assert(TimeInterval.Always.contains(Timestamp.Epoch - 1.ms))
    assert(TimeInterval.Always.contains(Timestamp.now + 100*365*24.h))

    assert(!TimeInterval.Always.endsBefore(Timestamp.Epoch - 100*365*24.h))
    assert(!TimeInterval.Always.endsBefore(Timestamp.Epoch))
    assert(!TimeInterval.Always.endsBefore(Timestamp.now))
    assert(!TimeInterval.Always.endsBefore(Timestamp.now + 100*365*24.h))
