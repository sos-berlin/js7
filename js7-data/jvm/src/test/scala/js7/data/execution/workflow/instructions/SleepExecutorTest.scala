package js7.data.execution.workflow.instructions

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.execution.workflow.instructions.SleepExecutor.bigDecimalSecondsToDuration
import scala.concurrent.duration.FiniteDuration

final class SleepExecutorTest extends OurTestSuite:

  "bigDecimalSecondsToDuration" in:
    assert(bigDecimalSecondsToDuration(BigDecimal(Long.MaxValue) + 1) == 1.h * 24 * 365 * 100)
    assert(bigDecimalSecondsToDuration(-1) == 0.s)
    assert(bigDecimalSecondsToDuration(1.234567890) == 1234567890.ns)
