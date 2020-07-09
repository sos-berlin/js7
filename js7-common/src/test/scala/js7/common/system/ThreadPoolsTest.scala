package js7.common.system

import js7.common.configutils.Configs._
import js7.common.system.ThreadPools._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ThreadPoolsTest extends AnyFreeSpec
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
