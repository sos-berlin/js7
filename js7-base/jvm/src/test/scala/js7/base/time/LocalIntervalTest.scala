package js7.base.time

import java.time.{LocalDateTime, ZoneId}
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts

final class LocalIntervalTest extends OurTestSuite:
  private val localInterval = LocalInterval(LocalDateTime.parse("2021-08-30T01:00"), 2.h)

  "endsBefore" in:
    assert(!localInterval.endsBefore(LocalDateTime.parse("2021-08-30T02:59:59")))
    assert(localInterval.endsBefore(LocalDateTime.parse("2021-08-30T03:00")))

  "contains" in:
    assert(!localInterval.contains(LocalDateTime.parse("2021-08-30T00:59:59")))
    assert(localInterval.contains(LocalDateTime.parse("2021-08-30T01:00")))
    assert(localInterval.contains(LocalDateTime.parse("2021-08-30T02:59:59")))
    assert(!localInterval.contains(LocalDateTime.parse("2021-08-30T03:00")))

  "toTimeInterval" in:
    assert(localInterval.toTimeInterval(ZoneId.of("Europe/Mariehamn")) ==
      TimeInterval(ts"2021-08-29T22:00:00Z", 2.h))
