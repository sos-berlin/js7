package com.sos.scheduler.engine.common.sprayutils

import akka.util.ByteString
import com.sos.scheduler.engine.common.sprayutils.XmlBytesTest._
import java.nio.charset.StandardCharsets.UTF_8
import org.scalatest.FreeSpec
import spray.http.HttpEntity
import spray.http.MediaTypes._
import spray.http.StatusCodes.OK
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.routing.Directives._
import spray.routing.Route
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class XmlBytesTest extends FreeSpec with ScalatestRouteTest {

  private val xmlBytes = XmlBytes(ByteString("<?xml encoding='utf-8'?><a><Ä/></a>".getBytes(UTF_8)))

  "Marshal as application/xml" in {
    val entity = HttpEntity(`application/xml`, xmlBytes.byteString)
    assert(marshal(xmlBytes) == Right(entity))
  }

  "Unmarshal application/xml" in {
    val entity = HttpEntity(`application/xml`, xmlBytes.byteString)
    assert(entity.as[XmlBytes] == Right(xmlBytes))
  }

  "Web service" in {
    Post("/", xmlBytes) ~> testRoute(xmlBytes) ~> check {
      assert(status == OK)
    }
  }
}

private object XmlBytesTest {
  private def testRoute(expected: XmlBytes): Route =
    entity(as[XmlBytes]) { entity ⇒
      assert(entity == expected)
      complete(OK)
    }
}
