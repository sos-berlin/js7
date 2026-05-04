package js7.base.log.reader

import java.time.Instant
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.log.LogLevel.Info
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class KeyedLogLineTest extends OurTestSuite:

  private val keyedLogLine = KeyedLogLine(
    logLevel = Info,
    fileInstant = Instant.parse("2026-04-30T00:00:00Z"),
    position = 1112223334445556667L,
    line = "LINE\n")

  "asString, parse" in :
    val string = "Info/1777507200/1112223334445556667 LINE\n"
    assert(keyedLogLine.asString == string)
    assert(KeyedLogLine.parse(string) == Right(keyedLogLine))
    assert(KeyedLogLine.parse("Info/1/2") == Left(Problem("Invalid KeyedLogLine format")))

  "JSON" in:
    testJson(
      keyedLogLine,
      json"""[
        "Info/1777507200/1112223334445556667",
        "LINE\n"
      ]""")
