package js7.base.utils

import scribe.{LogRecord, LoggerSupport}

object ScribeUtils
{
  lazy val emptyLogger: scribe.LoggerSupport =
    new LoggerSupport {
      def log[M](record: LogRecord[M]) = ()
    }
}
