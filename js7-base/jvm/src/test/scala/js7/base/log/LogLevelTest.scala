package js7.base.log

import org.scalatest.freespec.AnyFreeSpec
import scala.math.Ordering.Implicits.*
import LogLevel.ordering

final class LogLevelTest extends AnyFreeSpec {
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
