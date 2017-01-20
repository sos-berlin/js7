package com.sos.scheduler.engine.minicom.types

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class HRESULTTest extends FreeSpec {

  "toHex" in {
    assert(HRESULT(0x89abcdef).toHex == "89ABCDEF")
  }

  "comString" in {
    assert(HRESULT(0x89abcdef).comString == "COM-89ABCDEF")
  }

  "toString" in {
    assert(HRESULT(0x89abcdef).toString == "HRESULT(0x89ABCDEF)")
  }
}
