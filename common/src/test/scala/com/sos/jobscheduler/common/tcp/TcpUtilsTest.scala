package js7.common.tcp

import js7.common.tcp.TcpUtils.{parseTcpPort, requireTcpPortNumber}
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class TcpUtilsTest extends AnyFreeSpec {

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

  "requireTcpPortNumber" in {
    assert(requireTcpPortNumber(1) == 1)
    assert(requireTcpPortNumber(65535) == 65535)
    intercept[IllegalArgumentException] { requireTcpPortNumber(0) }
    intercept[IllegalArgumentException] { requireTcpPortNumber(-1) }
    intercept[IllegalArgumentException] { requireTcpPortNumber(65536) }
    intercept[IllegalArgumentException] { requireTcpPortNumber(99999) }
  }
}
