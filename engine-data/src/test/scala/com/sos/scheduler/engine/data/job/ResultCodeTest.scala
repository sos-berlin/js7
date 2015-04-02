package com.sos.scheduler.engine.data.job

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ResultCodeTest extends FreeSpec {

  "ResultCode Boolean/Int conversion" in {
    assert(ResultCode(0).isSuccess)
    assert(!ResultCode(1).isSuccess)
    assert(!ResultCode(3).isSuccess)
    assert(!ResultCode(-1).isSuccess)
    assert(ResultCode(true).isSuccess)
    assert(ResultCode(true).value == 0)
    assert(!ResultCode(false).isSuccess)
    assert(ResultCode(false).value == 1)
  }
}
