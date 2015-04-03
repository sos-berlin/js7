package com.sos.scheduler.engine.data.job

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
    assert(ReturnCode(true).toInt == 0)
    assert(!ReturnCode(false).isSuccess)
    assert(ReturnCode(false).toInt == 1)
  }

  "Success" in {
    assert(ReturnCode.Success.isSuccess)
  }

  "StandardSuccess" in {
    assert(!ReturnCode.StandardFailure.isSuccess)
  }
}
