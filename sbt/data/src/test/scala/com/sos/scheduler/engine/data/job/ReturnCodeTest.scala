package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ReturnCodeTest extends FreeSpec {

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

  "normalized C++ ReturnCode" in {
    assert(ReturnCode(-9).normalized == ReturnCode(128 + 9))
    assert(ReturnCode(-9).normalized == ReturnCode(SIGKILL))
    for (i ← List(0, 100, 200, 300)) assert(ReturnCode(i).normalized == ReturnCode(i))
  }
}
