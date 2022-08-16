package js7.base.io.process

import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.test.OurTestSuite

/**
 * @author Joacim Zschimmer
 */
final class ReturnCodeTest extends OurTestSuite {

  "Int ReturnCode" in {
    assert(ReturnCode(0).isSuccess)
    assert(!ReturnCode(1).isSuccess)
    assert(!ReturnCode(3).isSuccess)
    assert(!ReturnCode(-1).isSuccess)
  }

  "Boolean ReturnCode" in {
    assert(ReturnCode(true).isSuccess)
    assert(ReturnCode(true).number == 0)
    assert(!ReturnCode(false).isSuccess)
    assert(ReturnCode(false).number == 1)
  }

  "Success" in {
    assert(ReturnCode.Success.isSuccess)
  }

  "StandardSuccess" in {
    assert(!ReturnCode.StandardFailure.isSuccess)
  }

  "Interrupted process" in {
    assert(ReturnCode(SIGKILL) == ReturnCode(128 + 9))
    assert(ReturnCode(SIGTERM) == ReturnCode(128 + 15))
  }

  "pretty" in {
    assert(ReturnCode(1).pretty(isWindows = false) == "ReturnCode(1)")
    assert(ReturnCode(1).pretty(isWindows = true) == "ReturnCode(1)")
    assert(ReturnCode(143).pretty(isWindows = false) == "ReturnCode(143/SIGTERM)")
    assert(ReturnCode(143).pretty(isWindows = true) == "ReturnCode(143)")
    assert(ReturnCode(255).pretty(isWindows = false) == "ReturnCode(255=128+127)")
  }
}
