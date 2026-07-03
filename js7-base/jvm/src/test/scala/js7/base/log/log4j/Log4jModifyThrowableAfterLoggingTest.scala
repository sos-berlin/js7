package js7.base.log.log4j

import js7.base.test.OurTestSuite
import org.apache.logging.log4j.LogManager

final class Log4jModifyThrowableAfterLoggingTest extends OurTestSuite:

  "test" in:
    final class MyException extends Exception:
      private var i = 0
      override def hashCode(): Int =
        i += 1
        i

    val logger = LogManager.getLogger("TestLog4jNullPointerException")
    logger.error("TEST", new MyException)
