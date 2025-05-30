package js7.base.time

import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts

final class JavaTimestampTest extends OurTestSuite:

  assert(Timestamp.implementation == JavaTimestamp)

  private val timestamp = JavaTimestamp.parse("2021-08-29T22:00:00Z")
  private val local = LocalDateTime.parse("2021-08-30T01:00")
  private val zone = ZoneId.of("Europe/Mariehamn")

  "parse" in:
    assert(ts"2021-08-29T22:00:00Z" == timestamp)
    assert(ts"2021-08-29T22:00:00.1Z" == timestamp + 100.ms)
    assert(ts"2021-08-29T22:00:00.001Z" == timestamp + 1.ms)
    assert(ts"2021-08-29T22:00:00.999Z" == timestamp + 999.ms)
    assert(ts"2021-08-29T22:00:00.999999Z" == timestamp + 999.ms)

  "toString" in:
    assert(ts"1960-01-01T00:00:00Z".toString == "1960-01-01T00:00:00Z")
    assert(ts"1960-12-31T23:59:59.999Z".toString == "1960-12-31T23:59:59.999Z")
    assert(ts"2021-08-29T22:00:00Z".toString == "2021-08-29T22:00:00Z")
    assert(ts"2021-08-29T22:00:00.1Z".toString == "2021-08-29T22:00:00.100Z")
    assert(ts"2021-08-29T22:00:00.999Z".toString == "2021-08-29T22:00:00.999Z")

  "toInstant" in:
    assert(timestamp.toInstant == Instant.parse("2021-08-29T22:00:00Z"))

  "toLocalDateTime" in:
    assert(timestamp.toLocalDateTime(using zone) == local)

  "toZonedDateTime" in:
    assert(timestamp.toZonedDateTime(using zone) == ZonedDateTime.of(local, zone))

  "toOffsetDateTime" in:
    assert(timestamp.toOffsetDateTime(using zone) == OffsetDateTime.of(local, ZoneOffset.of("+03:00")))

  "local" in:
    assert(JavaTimestamp.local("2021-10-01T00:00")(using zone) == ts"2021-09-30T21:00:00Z")
