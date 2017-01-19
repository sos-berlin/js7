package com.sos.scheduler.engine.base.sprayjson

import com.sos.scheduler.engine.base.utils.Ascii.isAsciiDigit
import java.net.{InetAddress, InetSocketAddress}
import spray.json.{JsString, JsValue, JsonFormat}

/**
 * Serializes InetAddress as host name or IP number.
 *
 * @author Joacim Zschimmer
 */
object InetAddressJsonSupport {
  implicit object InetAddressJsonFormat extends JsonFormat[InetAddress] {

    def write(o: InetAddress) = JsString(o.getHostAddress)

    def read(jsValue: JsValue) = jsValue match {
      case JsString(string) ⇒ InetAddress.getByName(string)
      case _ ⇒ sys.error(s"String with IP number expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }

  implicit object InetSocketAddressJsonFormat extends JsonFormat[InetSocketAddress] {

    def write(o: InetSocketAddress) = {
      val inetAddress = o.getAddress
      if (inetAddress == null) throw new NullPointerException(s"No IP number in '$o'")
      JsString(s"${inetAddress.getHostAddress}:${o.getPort}")
    }

    def read(jsValue: JsValue) = jsValue match {
      case JsString(string) ⇒
        val List(host, port) = (string split ':').toList
        if (!isIpNumber(host)) throw new IllegalArgumentException(s"Not an IP number: $string")  // Do not lookup DNS
        new InetSocketAddress(host, port.toInt)
      case _ ⇒ sys.error(s"String with 'host:port' expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }

  private def isIpNumber(p: String) = p.nonEmpty && (isAsciiDigit(p(0)) || p(0) == '[' || p(0) == ':')
}
