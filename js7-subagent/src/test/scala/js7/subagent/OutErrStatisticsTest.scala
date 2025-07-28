package js7.subagent

import cats.effect.IO
import js7.base.test.OurAsyncTestSuite

final class OutErrStatisticsTest extends OurAsyncTestSuite:

  "OrderStdWrittenStatisticsMXBean" in:
    OutErrStatistics.registerMXBean.surround:
      IO.pure(succeed)
