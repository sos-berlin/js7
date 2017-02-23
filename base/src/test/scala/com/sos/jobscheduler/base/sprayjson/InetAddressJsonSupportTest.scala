package com.sos.scheduler.engine.base.sprayjson

import com.sos.scheduler.engine.base.sprayjson.InetAddressJsonSupport._
import java.net.{InetAddress, InetSocketAddress}
import org.scalatest.FreeSpec
import spray.json._


/**
 * @author Joacim Zschimmer
 */
final class InetAddressJsonSupportTest extends FreeSpec {

  "InetAddress" - {
    "IP only InetAddress" in {
      check(JsString("127.1.2.3"))
      check(JsString("1080:0:0:0:8:800:200c:417a"))
      checkDeserializationOnly("1080::8:800:200c:417a")
    }

    "Named InetAddress is serialized as IP number only" in {
      assert(InetAddress.getByName("localhost").toJson == JsString("127.0.0.1"))  // If IPv4
    }

    def check(json: JsString): Unit = {
      val inetAddress = InetAddress.getByName(json.value)
      assert(inetAddress.toJson == json)
      assert(inetAddress == json.convertTo[InetAddress])
    }

    def checkDeserializationOnly(string: String): Unit = {
      val json = JsString(string)
      val inetAddress = InetAddress.getByName(string)
      assert(inetAddress == json.convertTo[InetAddress])
    }
  }

  "InetSocketAddress" in {
    val json = JsString("127.1.2.3:777")
    val inetSocketAddress = new InetSocketAddress(InetAddress.getByName("127.1.2.3"), 777)
    assert(inetSocketAddress.toJson == json)
    assert(inetSocketAddress == json.convertTo[InetSocketAddress])
  }
}
