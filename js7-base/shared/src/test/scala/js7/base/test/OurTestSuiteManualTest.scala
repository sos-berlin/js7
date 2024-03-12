package js7.base.test

final class OurTestSuiteManualTest extends OurTestSuite:

  "test" in:
    info("OurTestSuiteManualTest: test")

  if false then
  "failing" in:
    fail()

  "pending" in:
    pending

  "is pending" is pending

  "ignore" ignore:
    sys.error("Not used")
