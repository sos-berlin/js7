package js7.base.log

import js7.base.log
import js7.base.test.OurTestSuite
import js7.base.utils.Ordinal.syntax.*
import scala.math.Ordering.Implicits.*

final class LogLevelTest extends OurTestSuite:

  "Ordering" in:
    val none: LogLevel = LogLevel.None
    val error: LogLevel = LogLevel.Error
    val warn: LogLevel = LogLevel.Warn
    val info: LogLevel = LogLevel.Info
    val debug: LogLevel = LogLevel.Debug
    val trace: LogLevel = LogLevel.Trace

    assert(LogLevel.MaxValue == none)
    assert(none > error)
    assert(error > warn)
    assert(warn > info)
    assert(info > debug)
    assert(debug > trace)
    assert(trace == LogLevel.MinValue)

  "LogLevel(String)" in:
    assert(LogLevel("none") eq LogLevel.None)
    assert(LogLevel("error") eq LogLevel.Error)
    assert(LogLevel("warn") eq LogLevel.Warn)
    assert(LogLevel("info") eq LogLevel.Info)
    assert(LogLevel("debug") eq LogLevel.Debug)
    assert(LogLevel("trace") eq LogLevel.Trace)

  "NoneIsLowestOrdinal" - {
    import LogLevel.NoneIsLowestOrdinal
    "pred" in:
      def check(pair: (LogLevel, LogLevel)) = assert(pair._1.pred == pair._2)
      import LogLevel.*

      check(Trace -> None)
      check(Debug -> Trace)
      check(Info -> Debug)
      check(Warn -> Info)
      check(Error -> Warn)

    "succ" in:
      def check(pair: (LogLevel, LogLevel)) = assert(pair._1.succ == pair._2)
      import LogLevel.*

      check(None -> Trace)
      check(Trace -> Debug)
      check(Debug -> Info)
      check(Info -> Warn)
      check(Warn -> Error)
      check(Error -> Error)
  }

  "NoneIsHighestOrdinal" - {
    import LogLevel.NoneIsHighestOrdinal
    "pred" in:
      def check(pair: (LogLevel, LogLevel)) = assert(pair._1.pred == pair._2)
      import LogLevel.*

      check(Trace -> Trace)
      check(Debug -> Trace)
      check(Info -> Debug)
      check(Warn -> Info)
      check(Error -> Warn)
      check(None -> Error)

    "succ" in:
      def check(pair: (LogLevel, LogLevel)) = assert(pair._1.succ == pair._2)
      import LogLevel.*

      check(Trace -> Debug)
      check(Debug -> Info)
      check(Info -> Warn)
      check(Warn -> Error)
      check(Error -> None)
      check(None -> None)
  }
