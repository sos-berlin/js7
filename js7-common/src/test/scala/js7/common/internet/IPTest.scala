package js7.common.internet

import java.net.{InetAddress, InetSocketAddress, UnknownHostException}
import js7.base.convert.As.convert
import js7.common.internet.IP.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IPTest extends AnyFreeSpec {

  "InetAddress" in {
    intercept[IllegalArgumentException] { convert[String, InetAddress]("") }
    intercept[UnknownHostException] { convert[String, InetAddress](" ") }
    assert(convert[String, InetAddress]("1.2.3.4") == InetAddress.getByName("1.2.3.4"))
    assert(convert[String, InetAddress]("localhost") == InetAddress.getByName("localhost"))
  }

  "InetSocketAddress" in {
    intercept[IllegalArgumentException] { convert[String, InetSocketAddress]("") }
    intercept[IllegalArgumentException] { convert[String, InetSocketAddress](" ") }
    intercept[IllegalArgumentException] { convert[String, InetSocketAddress](" :1") }
    intercept[IllegalArgumentException] { convert[String, InetSocketAddress]("127.0.0.1") }
    intercept[IllegalArgumentException] { convert[String, InetSocketAddress]("localhost") }
    intercept[IllegalArgumentException] { convert[String, InetSocketAddress]("::1") }
    intercept[IllegalArgumentException] { convert[String, InetSocketAddress]("1:2:3:4:5:6:12345") }
    assert(convert[String, InetSocketAddress]("1.2.3.4:12345") == new InetSocketAddress("1.2.3.4", 12345))
    assert(convert[String, InetSocketAddress]("localhost:12345") == new InetSocketAddress("localhost", 12345))
    assert(convert[String, InetSocketAddress]("[1:2:3:4:5:6]:12345") == new InetSocketAddress("1:2:3:4:5:6", 12345))
    assert(convert[String, InetSocketAddress]("[::1]:12345") == new InetSocketAddress("::1", 12345))
  }

  "StringToInetSocketAddress with default" - {
    def use(defaultHost: String, defaultPort: Option[Int])(body: (String => InetSocketAddress) => Unit): Unit =
      s"With defaults $defaultHost and $defaultPort" in {
        body { string => toInetSocketAddress(string, defaultHost, defaultPort) }
      }

    use("0.0.0.0", None) { c =>
      intercept[IllegalArgumentException] { c("") }
      assert(c("1.2.3.4:9999") == new InetSocketAddress("1.2.3.4", 9999))
      assert(c("[1:2:3:4:5:6]:9999") == new InetSocketAddress("1:2:3:4:5:6", 9999))
      intercept[IllegalArgumentException] { c("127.0.0.1") }
      assert(c("9999") == new InetSocketAddress("0.0.0.0", 9999))
    }

    use("127.0.0.1", Some(1111)) { c =>
      assert(c("") == new InetSocketAddress("127.0.0.1", 1111))
      assert(c("1.2.3.4:9999") == new InetSocketAddress("1.2.3.4", 9999))
      assert(c("[1:2:3:4:5:6]:9999") == new InetSocketAddress("1:2:3:4:5:6", 9999))
      assert(c("127.0.0.1") == new InetSocketAddress("127.0.0.1", 1111))
      assert(c("9999") == new InetSocketAddress("127.0.0.1", 9999))
    }

    use("127.0.0.1", None) { c =>
      intercept[IllegalArgumentException] { c("") }
      assert(c("1.2.3.4:9999") == new InetSocketAddress("1.2.3.4", 9999))
      assert(c("[1:2:3:4:5:6]:9999") == new InetSocketAddress("1:2:3:4:5:6", 9999))
      intercept[IllegalArgumentException] { c("127.0.0.1") }
      assert(c("9999") == new InetSocketAddress("127.0.0.1", 9999))
    }

    use("0.0.0.0", Some(1111)) { c =>
      assert(c("") == new InetSocketAddress("0.0.0.0", 1111))
      assert(c("1.2.3.4:9999") == new InetSocketAddress("1.2.3.4", 9999))
      assert(c("[1:2:3:4:5:6]:9999") == new InetSocketAddress("1:2:3:4:5:6", 9999))
      assert(c("1.2.3.4") == new InetSocketAddress("1.2.3.4", 1111))
      assert(c("[1:2:3:4:5:6]") == new InetSocketAddress("1:2:3:4:5:6", 1111))
      assert(c("9999") == new InetSocketAddress("0.0.0.0", 9999))
    }
  }
}
