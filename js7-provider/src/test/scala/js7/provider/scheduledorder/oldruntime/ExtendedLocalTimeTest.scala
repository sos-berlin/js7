package js7.provider.scheduledorder.oldruntime

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneId}
import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class ExtendedLocalTimeTest extends Test {

  "fromString" - {
    "Invalid times" in {
      intercept[IllegalArgumentException] { ExtendedLocalTime.fromString("") }
      intercept[IllegalArgumentException] { ExtendedLocalTime.fromString("0") }
      intercept[IllegalArgumentException] { ExtendedLocalTime.fromString("0:0:0:0") }
      intercept[IllegalArgumentException] { ExtendedLocalTime.fromString("0:0") }
      intercept[IllegalArgumentException] { ExtendedLocalTime.fromString("0:00:0") }
      intercept[IllegalArgumentException] { ExtendedLocalTime.fromString("-0:0") }
      intercept[IllegalArgumentException] { ExtendedLocalTime.fromString("0:60") }
      intercept[IllegalArgumentException] { ExtendedLocalTime.fromString("0:00:60") }
    }

    for ((string, normalized, nano) <- Array(
      ("0:00"     ,  "0:00:00", 0L),
      ("000:00:00",  "0:00:00", 0L),
      ("12:00"    , "12:00:00", LocalTime.of(12,  0,  0).toNanoOfDay),
      ("12:10:09" , "12:10:09", LocalTime.of(12, 10,  9).toNanoOfDay),
      ("12:09:10" , "12:09:10", LocalTime.of(12,  9, 10).toNanoOfDay),
      ("23:59:59" , "23:59:59", LocalTime.of(23, 59, 59).toNanoOfDay),
      ("48:00"    , "48:00:00",  48L*60*60*1000*1000*1000))
    ) string in {
      val t = ExtendedLocalTime.fromString(string)
      assert(t.toNanoOfDay == nano)
      assert(t.toString == normalized)
    }
  }

  "toInstant" in {
    assert(ExtendedLocalTime.fromString("08:00").toInstant(LocalDate.of(2017, 1, 1), ZoneId.of("Europe/Berlin")) ==
      Instant.parse("2017-01-01T07:00:00Z"))
    assert(ExtendedLocalTime.fromString("08:00").toInstant(LocalDate.of(2017, 8, 1), ZoneId.of("Europe/Berlin")) ==
      Instant.parse("2017-08-01T06:00:00Z"))
  }


  "toLocalDate" in {
    assert(ExtendedLocalTime.fromString("32:00").toLocalDateTime(LocalDate.of(2017, 1, 1)) ==
      LocalDateTime.of(2017, 1, 2, 8, 0, 0))
  }

  "toLocalTime" in {
    assert(ExtendedLocalTime.fromString("32:30:59").localTime ==
      LocalTime.of(8, 30, 59))
  }

  "compare" in {
    assert(ExtendedLocalTime.fromString("01:00") < ExtendedLocalTime.fromString("02:00"))
  }

  "StartOfDay" in {
    assert(ExtendedLocalTime.StartOfDay.toNanoOfDay == 0)
  }

  "EndOfDay" in {
    assert(ExtendedLocalTime.EndOfDay.toNanoOfDay == 24L*60*60*1000*1000*1000)
  }
}
