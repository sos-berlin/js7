package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.XmlStringTest._
import java.nio.charset.StandardCharsets.UTF_8
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpCharsets.`UTF-8`
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
@RunWith(classOf[JUnitRunner])
final class XmlStringTest extends FreeSpec with ScalatestRouteTest {

  private val xmlString = XmlString("<a><Ä/></a>")
  private val bytes = xmlString.string.getBytes(UTF_8)

  "Marshal as application/xml" in {
    val entity = HttpEntity(`application/xml`, bytes)
    assert(marshal(xmlString) == Right(entity))
  }

  "Unmarshal application/xml" in {
    val entity = HttpEntity(`application/xml`, bytes)
    assert(entity.as[XmlString] == Right(xmlString))
  }

  "Unmarshal text/xml" in {
    val entity = HttpEntity(`text/xml` withCharset `UTF-8`, bytes)
    assert(entity.as[XmlString] == Right(xmlString))
  }

  "Web service" in {
    Post("/", xmlString) ~> testRoute(xmlString) ~> check {
      assert(status == OK)
    }
  }
}

private object XmlStringTest {
  private def testRoute(expected: XmlString): Route =
    entity(as[XmlString]) { entity ⇒
      assert(entity == expected)
      complete(OK)
    }
}
