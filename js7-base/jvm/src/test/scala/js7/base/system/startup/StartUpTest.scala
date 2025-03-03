package js7.base.system.startup

import java.time.{ZoneId, ZonedDateTime}
import js7.base.test.OurTestSuite

final class StartUpTest extends OurTestSuite:

  "Just log" in:
    Logger[this.type].info(StartUp.startUpLine("StartUpTest"))
    StartUp.logJavaSettings()


  "zonedDateTimeToString" in:
     val zonedDateTime =
       ZonedDateTime.of(2025, 3, 3, 12, 0, 0, 123999999, ZoneId.of("Europe/Mariehamn"))
     assert(StartUp.zonedDateTimeToString(zonedDateTime) == "2025-03-03 12:00:00.123+0200")
