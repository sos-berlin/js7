package js7.common.system

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.common.system.ThreadPools.*

/**
  * @author Joacim Zschimmer
  */
final class ThreadPoolsTest extends OurTestSuite
{
  "ThreadCount" - {
    val config = config"""
      simple = 3
      factor = 2.5x"""

    "Simple number" in {
      assert(config.as("simple")(ThreadCount) == 3)
    }

    "Factor" in {
      assert(config.as("factor")(ThreadCount) == (2.5 * sys.runtime.availableProcessors).ceil.toInt)
    }
  }
}
