package js7.base.time

import js7.base.time.ScalaTime.*
import org.scalatest.freespec.AnyFreeSpec

final class TimeIntervalTest extends AnyFreeSpec
{
  private val timeInterval = TimeInterval(Timestamp("2021-08-23T00:00:00Z"), 1.h)

  "endsBefore" in {
    assert(!timeInterval.endsBefore(timeInterval.start - 1.ms))
    assert(!timeInterval.endsBefore(timeInterval.start))
    assert(!timeInterval.endsBefore(timeInterval.start + timeInterval.duration - 1.ms))
    assert(timeInterval.endsBefore(timeInterval.start + timeInterval.duration))
    assert(timeInterval.endsBefore(timeInterval.start + timeInterval.duration + 1.ms))
  }

  "contains" in {
    assert(!timeInterval.contains(timeInterval.start - 1.ms))
    assert(timeInterval.contains(timeInterval.start))
    assert(timeInterval.contains(timeInterval.start + timeInterval.duration - 1.ms))
    assert(!timeInterval.contains(timeInterval.start + timeInterval.duration))
  }

  "never" in {
    assert(!TimeInterval.never.contains(Timestamp.now))
    assert(!TimeInterval.never.contains(Timestamp.now - 100*365*24.h))
    assert(!TimeInterval.never.contains(Timestamp.now + 100*365*24.h))
    assert(TimeInterval.never.endsBefore(Timestamp.Epoch))
    assert(TimeInterval.never.endsBefore(Timestamp.now + 100*365*24.h))
  }

  "always" in {
    assert(TimeInterval.alwaysSinceEpoch.contains(Timestamp.now))
    assert(!TimeInterval.alwaysSinceEpoch.contains(Timestamp.Epoch - 1.ms))
    assert(TimeInterval.alwaysSinceEpoch.contains(Timestamp.now + 100*365*24.h))

    assert(!TimeInterval.alwaysSinceEpoch.endsBefore(Timestamp.Epoch - 100*365*24.h))
    assert(!TimeInterval.alwaysSinceEpoch.endsBefore(Timestamp.Epoch))
    assert(!TimeInterval.alwaysSinceEpoch.endsBefore(Timestamp.now))
    assert(!TimeInterval.alwaysSinceEpoch.endsBefore(Timestamp.now + 100*365*24.h))
  }
}
