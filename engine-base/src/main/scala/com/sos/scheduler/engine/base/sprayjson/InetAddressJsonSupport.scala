package com.sos.scheduler.engine.base.sprayjson

import java.net.InetAddress
import spray.json.{JsString, JsValue, JsonFormat}

/**
 * Serializes InetAddress as IP number.
 * The host name is discarded.
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
}
