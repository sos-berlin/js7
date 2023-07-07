package js7.base.log

import js7.base.log.LogLevel.ordering
import js7.base.test.OurTestSuite
import scala.math.Ordering.Implicits.*

final class LogLevelTest extends OurTestSuite {
  "Ordering" in {
    val logNone: LogLevel = LogLevel.LogNone
    val trace: LogLevel = LogLevel.Trace
    val debug: LogLevel = LogLevel.Debug
    val info: LogLevel = LogLevel.Info
    val warn: LogLevel = LogLevel.Warn
    val error: LogLevel = LogLevel.Error

    assert(logNone < debug)
    assert(trace < debug)
    assert(debug < info)
    assert(info < warn)
    assert(warn < error)
  }
}
