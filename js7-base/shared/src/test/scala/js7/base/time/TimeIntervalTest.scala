package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimeInterval.{Always, Never}
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

  "tryCombine" in:
    def testCombine(a: TimeInterval, b: TimeInterval, expected: Option[TimeInterval]): Unit =
      assert(a.tryCombine(b) == expected)
      assert(b.tryCombine(a) == expected)

    testCombine(timeInterval, timeInterval, Some(timeInterval))
    testCombine(timeInterval, TimeInterval(ts"2021-08-23T00:00:00Z", 2.h),
      Some(TimeInterval(ts"2021-08-23T00:00:00Z", 2.h)))
    testCombine(timeInterval, TimeInterval(ts"2021-08-23T00:12:34Z", 1.h),
      Some(TimeInterval(ts"2021-08-23T00:00:00Z", 1.h + 12.minutes + 34.s)))
    testCombine(timeInterval, TimeInterval(ts"2021-08-23T01:00:00Z", 2.h),
      Some(TimeInterval(ts"2021-08-23T00:00:00Z", 3.h)))
    testCombine(timeInterval, TimeInterval(ts"2021-08-23T01:00:01Z", 2.h),
      None)

    testCombine(Always, timeInterval, Some(Always))
    testCombine(Always, Always, Some(Always))
    testCombine(Always, Never, Some(Always))

    testCombine(Never, timeInterval, None)
    testCombine(Never, Never, None)

  "Never" in:
    assert(!Never.contains(Timestamp.now))
    assert(!Never.contains(Timestamp.now - 100*365*24.h))
    assert(!Never.contains(Timestamp.now + 100*365*24.h))
    assert(Never.endsBefore(Timestamp.Epoch))
    assert(Never.endsBefore(Timestamp.now + 100*365*24.h))

  "Always" in:
    assert(Always.contains(Timestamp.now))
    assert(Always.contains(Timestamp.Epoch - 1.ms))
    assert(Always.contains(Timestamp.now + 100*365*24.h))

    assert(!Always.endsBefore(Timestamp.Epoch - 100*365*24.h))
    assert(!Always.endsBefore(Timestamp.Epoch))
    assert(!Always.endsBefore(Timestamp.now))
    assert(!Always.endsBefore(Timestamp.now + 100*365*24.h))
