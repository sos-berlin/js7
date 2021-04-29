package js7.base.log

import org.slf4j.LoggerFactory

object Slf4jUtils
{
  // Tests only: Try to await slf4j initialization to avoid error messages like this:
  // SLF4J: (logger name here) ...
  // SLF4J: The following set of substitute loggers may have been accessed
  // SLF4J: during the initialization phase. Logging calls during this
  // SLF4J: phase were not honored. However, subsequent logging calls to these
  // SLF4J: loggers will work as normally expected.
  // SLF4J: See also http://www.slf4j.org/codes.html#substituteLogger
  LoggerFactory.getILoggerFactory()

  def initialize() = {}
}
