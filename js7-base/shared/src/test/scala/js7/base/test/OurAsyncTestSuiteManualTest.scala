package js7.base.test

import org.slf4j.LoggerFactory
import scala.concurrent.Future

final class OurAsyncTestSuiteManualTest extends OurAsyncTestSuite {
  private val logger = LoggerFactory.getLogger(getClass)

  "test" in {
    Future {
      Thread.sleep(10)
      logger.info("test")
      succeed
    }
  }

  if (false)
  "failing" in {
    Future {
      Thread.sleep(10)
      fail()
    }
  }

  "pending in Future (?)" in {
    Future {
      Thread.sleep(10)
      pending
    }
  }

  "pending" in {
    pending
  }

  "is pending" is pending

  "ignore" ignore {
    sys.error("Not used")
  }
}
