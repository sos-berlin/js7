package com.sos.jobscheduler.data.job

import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class ReturnCodeTest extends AnyFreeSpec {

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
}
