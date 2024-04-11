package js7.base.log

import js7.base.test.OurTestSuite
import scala.math.Ordering.Implicits.*

final class LogLevelTest extends OurTestSuite:

  "Ordering" in:
    val none: LogLevel = LogLevel.None
    val error: LogLevel = LogLevel.Error
    val warn: LogLevel = LogLevel.Warn
    val info: LogLevel = LogLevel.Info
    val debug: LogLevel = LogLevel.Debug
    val trace: LogLevel = LogLevel.Trace

    assert(none > error)
    assert(error > warn)
    assert(warn > info)
    assert(info > debug)
    assert(debug > trace)

  "LogLevel(String)" in:
    assert(LogLevel("none") eq LogLevel.None)
    assert(LogLevel("error") eq LogLevel.Error)
    assert(LogLevel("warn") eq LogLevel.Warn)
    assert(LogLevel("info") eq LogLevel.Info)
    assert(LogLevel("debug") eq LogLevel.Debug)
    assert(LogLevel("trace") eq LogLevel.Trace)
