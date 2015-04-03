package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.utils.TcpUtils.parseTcpPort
import com.sos.scheduler.engine.common.utils.TcpUtils.requireTcpPort
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TcpUtilsTest extends FreeSpec {

  "parseTcpPort" in {
    assert(parseTcpPort("1") == 1)
    assert(parseTcpPort("65535") == 65535)
    intercept[IllegalArgumentException] { parseTcpPort("0") }
    intercept[IllegalArgumentException] { parseTcpPort("-1") }
    intercept[IllegalArgumentException] { parseTcpPort("65536") }
    intercept[IllegalArgumentException] { parseTcpPort("99999") }
    intercept[IllegalArgumentException] { parseTcpPort(" 3000") }
    intercept[IllegalArgumentException] { parseTcpPort("3000 ") }
  }

  "requireTcpPort" in {
    assert(requireTcpPort(1) == 1)
    assert(requireTcpPort(65535) == 65535)
    intercept[IllegalArgumentException] { requireTcpPort(0) }
    intercept[IllegalArgumentException] { requireTcpPort(-1) }
    intercept[IllegalArgumentException] { requireTcpPort(65536) }
    intercept[IllegalArgumentException] { requireTcpPort(99999) }
  }
}
