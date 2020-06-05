package js7.core.system

import com.typesafe.config.ConfigFactory
import js7.common.configutils.Configs.ConvertibleConfig
import js7.core.system.ThreadPools._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ThreadPoolsTest extends AnyFreeSpec
{
  "ThreadCount" - {
    val config = ConfigFactory.parseString(
      """simple = 3
        |factor = 2.5x
      """.stripMargin)

    "Simple number" in {
      assert(config.as("simple")(ThreadCount) == 3)
    }

    "Factor" in {
      assert(config.as("factor")(ThreadCount) == (2.5 * sys.runtime.availableProcessors).ceil.toInt)
    }
  }
}
