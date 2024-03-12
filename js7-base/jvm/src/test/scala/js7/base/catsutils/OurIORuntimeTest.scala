package js7.base.catsutils

import js7.base.catsutils.OurIORuntime.ThreadCount
import org.scalatest.freespec.AnyFreeSpec

final class OurIORuntimeTest extends AnyFreeSpec:

  "ThreadCount" in:
    assert(ThreadCount("3") == 3)
    assert(ThreadCount("1/1") == (sys.runtime.availableProcessors * 1 / 1.0).ceil.toInt)
    assert(ThreadCount("2/4") == (sys.runtime.availableProcessors * 2 / 4.0).ceil.toInt)

    intercept[IllegalArgumentException]:
      ThreadCount("1/0")
