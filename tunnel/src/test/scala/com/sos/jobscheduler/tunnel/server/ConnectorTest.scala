package com.sos.scheduler.engine.tunnel.server

import akka.util.ByteString
import org.scalatest.FreeSpec
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
final class ConnectorTest extends FreeSpec {

  "Request.toString" in {
    val request = Connector.Request(ByteString(0, 1, 30, 31), Promise(), None)
    assert(request.toString == "Request(4 bytes 00 01 1e 1f)")
  }

  "Big Request.toString" in {
    val request = Connector.Request(ByteString.fromInts(0 until 1000: _*), Promise(), None)
    assert(request.toString startsWith "Request(1000 bytes 00 01 02 03 04 ")
    assert(request.toString.size < 330)
  }
}
