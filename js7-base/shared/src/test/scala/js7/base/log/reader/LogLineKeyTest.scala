package js7.base.log.reader

import java.time.Instant
import js7.base.problem.Problem
import js7.base.test.OurTestSuite

final class LogLineKeyTest extends OurTestSuite:

  "LogLineKey" in:
    val logLineKey = LogLineKey(Instant.parse("2026-04-30T00:00:00.123456789Z"), 1112223334445556667L)
    val string = "1777507200.123456789/1112223334445556667"

    assert(LogLineKey.parse(" " + string) == Left(Problem:
      "Invalid LogLineKey:  1777507200.123456789/1112223334445556667"))
    assert(LogLineKey.parse("") == Left(Problem("Invalid LogLineKey: ")))

    assert(logLineKey.toString == string)
    assert(LogLineKey.parse(string) == Right(logLineKey))

    assert:
      LogLineKey(Instant.parse("2026-04-30T00:00:00Z"), 0).toString == "1777507200/0"

    val legacy = "Info/1777507200.123456789/1112223334445556667"
    assert(LogLineKey.parse(legacy) == Right(logLineKey))
