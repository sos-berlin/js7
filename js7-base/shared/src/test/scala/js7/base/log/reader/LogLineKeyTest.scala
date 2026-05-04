package js7.base.log.reader

import java.time.Instant
import js7.base.log.LogLevel.Info
import js7.base.problem.Problem
import js7.base.test.OurTestSuite

final class LogLineKeyTest extends OurTestSuite:

  "LogLineKey" in:
    val logLineKey = LogLineKey(Info, Instant.parse("2026-04-30T00:00:00.123456789Z"), 1112223334445556667L)
    val string = "Info/1777507200.123456789/1112223334445556667"

    assert(LogLineKey.parse(" " + string) == Left(Problem:
      "Invalid LogLineKey:  Info/1777507200.123456789/1112223334445556667"))
    assert(LogLineKey.parse("") == Left(Problem("Invalid LogLineKey: ")))

    assert(logLineKey.toString == string)
    assert(LogLineKey.parse(string) == Right(logLineKey))

    assert:
      LogLineKey(Info, Instant.parse("2026-04-30T00:00:00Z"), 0).toString == "Info/1777507200/0"
