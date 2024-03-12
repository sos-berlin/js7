package js7.base.test

import scala.concurrent.Future

final class OurAsyncTestSuiteManualTest extends OurAsyncTestSuite:

  "test" in:
    Future:
      Thread.sleep(10)
      info("OurAsyncTestSuiteManualTest: test")
      succeed

  if false then
  "failing" in:
    Future:
      Thread.sleep(10)
      fail()

  "pending in Future (?)" in:
    Future:
      Thread.sleep(10)
      pending

  "pending" in:
    pending

  "is pending" is pending

  "ignore" ignore:
    sys.error("Not used")
