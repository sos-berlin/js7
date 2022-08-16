package js7.base.test

import org.slf4j.LoggerFactory

final class OurTestSuiteManualTest extends OurTestSuite {

  private val logger = LoggerFactory.getLogger(getClass)

  "test" in {
    logger.info("test")
  }

  if (false)
  "failing" in {
    fail()
  }

  "pending" in {
    pending
  }

  "is pending" is pending

  "ignore" ignore {
    sys.error("Not used")
  }
}
