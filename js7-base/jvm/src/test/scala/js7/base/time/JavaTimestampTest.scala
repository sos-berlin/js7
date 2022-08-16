package js7.base.time

import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimestamp.specific.*

final class JavaTimestampTest extends OurTestSuite
{
  private val timestamp = JavaTimestamp.parse("2021-08-29T22:00:00Z")
  private val local = LocalDateTime.parse("2021-08-30T01:00")
  private val zone = ZoneId.of("Europe/Mariehamn")

  "toInstant" in {
    assert(timestamp.toInstant == Instant.parse("2021-08-29T22:00:00Z"))
  }

  "toLocalDateTime" in {
    assert(timestamp.toLocalDateTime(zone) == local)
  }

  "toZonedDateTime" in {
    assert(timestamp.toZonedDateTime(zone) == ZonedDateTime.of(local, zone))
  }

  "toOffsetDateTime" in {
    assert(timestamp.toOffsetDateTime(zone) == OffsetDateTime.of(local, ZoneOffset.of("+03:00")))
  }

  "local" in {
    assert(JavaTimestamp.local("2021-10-01T00:00")(zone) == Timestamp("2021-09-30T21:00:00Z"))
  }
}
