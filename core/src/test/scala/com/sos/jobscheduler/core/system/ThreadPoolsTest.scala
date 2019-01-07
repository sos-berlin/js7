package com.sos.jobscheduler.core.system

import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.core.system.ThreadPools._
import com.typesafe.config.ConfigFactory
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ThreadPoolsTest extends FreeSpec
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
