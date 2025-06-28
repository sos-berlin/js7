package js7.base.time

import java.time.{LocalDateTime, ZoneId}
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts

final class LocalIntervalTest extends OurTestSuite:
  private given zoneId: ZoneId = ZoneId.of("Europe/Mariehamn")
  private val localInterval = LocalInterval(local("2021-08-30T01:00"), 2.h)

  "startsBefore" in:
    assert(!localInterval.startsBefore(local("2021-08-30T00:59")))
    assert(localInterval.startsBefore(local("2021-08-30T01:00")))

  "endsBefore" in:
    assert(!localInterval.endsBefore(local("2021-08-30T02:59:59")))
    assert(localInterval.endsBefore(local("2021-08-30T03:00")))

  "contains" in:
    assert(!localInterval.contains(local("2021-08-30T00:59:59")))
    assert(localInterval.contains(local("2021-08-30T01:00")))
    assert(localInterval.contains(local("2021-08-30T02:59:59")))
    assert(!localInterval.contains(local("2021-08-30T03:00")))

  "toTimeInterval" in:
    assert(localInterval.toTimeInterval == TimeInterval(ts"2021-08-29T22:00:00Z", 2.h))

  "Daylight-saving time" - {
    def localTS(local: String) = JavaTimestamp.local(local).toTimestamp

    "Begin" - {
      // LocalInterval#duration is the real duration.
      // Even when DST shifts, the duration is the same.
      // In spring, it is not shortened. In autumn, it is not extended.
      val localInterval = LocalInterval(local("2021-03-28T03:00"), 1.h)

      "Conversions" in:
        assert(ts"2021-03-28T00:00:00Z" == localTS("2021-03-28T02:00"))
        assert(ts"2021-03-28T01:00:00Z" == localTS("2021-03-28T03:00"))
        assert(ts"2021-03-28T01:00:00Z" == localTS("2021-03-28T04:00"))
        assert(ts"2021-03-28T01:59:00Z" == localTS("2021-03-28T03:59"))
        assert(ts"2021-03-28T02:00:00Z" == localTS("2021-03-28T05:00"))

        assert(localTS("2021-03-28T03:00") == localTS("2021-03-28T04:00"))

      "startsBefore" in:
        assert(!localInterval.startsBefore(local("2021-03-28T03:00")))
        assert(!localInterval.startsBefore(local("2021-03-28T03:59")))
        assert(localInterval.startsBefore(local("2021-03-28T04:00")))

        assert(!localInterval.startsBefore(ts"2021-03-28T00:59:59Z".toLocalDateTime))
        assert(localInterval.startsBefore(ts"2021-03-28T01:00:00Z".toLocalDateTime))
        assert(localInterval.startsBefore(ts"2021-03-28T01:59:59Z".toLocalDateTime))
        assert(localInterval.startsBefore(ts"2021-03-28T02:00:00Z".toLocalDateTime))

      "endsBefore" in:
        assert(!localInterval.endsBefore(local("2021-03-28T03:00")))
        assert(!localInterval.endsBefore(local("2021-03-28T04:00")))
        assert(!localInterval.endsBefore(local("2021-03-28T04:59")))
        assert(localInterval.endsBefore(local("2021-03-28T05:00")))

        assert(!localInterval.endsBefore(ts"2021-03-28T01:59:59Z".toLocalDateTime))
        assert(localInterval.endsBefore(ts"2021-03-28T02:00:00Z".toLocalDateTime))
        assert(localInterval.endsBefore(ts"2021-03-28T02:59:59Z".toLocalDateTime))
        assert(localInterval.endsBefore(ts"2021-03-28T03:00:00Z".toLocalDateTime))

      "contains" in:
        assert(localInterval.contains(local("2021-03-28T03:00")))
        assert(localInterval.contains(local("2021-03-28T03:59")))
        assert(localInterval.contains(local("2021-03-28T04:00")))  // because 03:00 = 04:00
        assert(localInterval.contains(local("2021-03-28T04:59")))
        assert(!localInterval.contains(local("2021-03-28T05:00")))

        assert(!localInterval.contains(ts"2021-03-28T00:59:59Z".toLocalDateTime))
        assert(localInterval.contains(ts"2021-03-28T01:00:00Z".toLocalDateTime))
        assert(localInterval.contains(ts"2021-03-28T01:59:59Z".toLocalDateTime))
        assert(!localInterval.contains(ts"2021-03-28T02:00:00Z".toLocalDateTime))
        assert(!localInterval.contains(ts"2021-03-28T03:00:00Z".toLocalDateTime))
    }

    "End" - {
      val localInterval = LocalInterval(local("2021-10-31T03:00"), 1.h)

      "Conversions" in:
        assert(ts"2021-10-30T23:00:00Z" == localTS("2021-10-31T02:00"))
        assert(ts"2021-10-31T00:00:00Z" == localTS("2021-10-31T03:00"))
        assert(ts"2021-10-31T00:59:00Z" == localTS("2021-10-31T03:59"))
        assert(ts"2021-10-31T02:00:00Z" == localTS("2021-10-31T04:00"))
        assert(ts"2021-10-31T03:00:00Z" == localTS("2021-10-31T05:00"))

        assert(localTS("2021-10-31T02:00") + 3.h == JavaTimestamp.local("2021-10-31T04:00"))

      "startsBefore" in:
        assert(!localInterval.startsBefore(local("2021-10-31T02:59")))
        assert(localInterval.startsBefore(local("2021-10-31T03:00")))

        assert(!localInterval.startsBefore(ts"2021-10-30T23:59:59Z".toLocalDateTime))
        assert(localInterval.startsBefore(ts"2021-10-31T00:00:00Z".toLocalDateTime))

      "endsBefore" in:
        assert(!localInterval.endsBefore(local("2021-10-31T03:00")))
        assert(!localInterval.endsBefore(local("2021-10-31T03:59")))
        assert(localInterval.endsBefore(local("2021-10-31T04:00")))
        assert(localInterval.endsBefore(local("2021-10-31T05:00")))

        assert(!localInterval.endsBefore(ts"2021-10-31T01:59:59Z".toLocalDateTime))
        assert(localInterval.endsBefore(ts"2021-10-31T02:00:00Z".toLocalDateTime))

      "contains" in:
        assert(!localInterval.contains(local("2021-10-31T02:59")))
        assert(localInterval.contains(local("2021-10-31T03:00")))
        assert(localInterval.contains(local("2021-10-31T03:59")))
        assert(!localInterval.contains(local("2021-10-31T04:00")))
        assert(!localInterval.contains(local("2021-10-31T05:00")))

        // Two real hours are included in localDuration, despite duration = 1.h !!!
        assert(!localInterval.contains(ts"2021-10-30T23:59:59Z".toLocalDateTime))
        assert(localInterval.contains(ts"2021-10-31T00:00:00Z".toLocalDateTime))
        assert(localInterval.contains(ts"2021-10-31T01:00:00Z".toLocalDateTime))
        assert(localInterval.contains(ts"2021-10-31T01:59:59Z".toLocalDateTime))
        assert(!localInterval.contains(ts"2021-10-31T02:00:00Z".toLocalDateTime))
    }
  }

  private def local(local: String) = LocalDateTime.parse(local)
