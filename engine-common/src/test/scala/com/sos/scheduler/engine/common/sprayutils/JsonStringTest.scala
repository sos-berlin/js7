package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.JsonStringTest._
import java.nio.charset.StandardCharsets.UTF_8
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpCharsets.`UTF-8`
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.StatusCodes.OK
import spray.http.{ContentType, HttpEntity}
import spray.httpx.marshalling._
import spray.routing.Directives._
import spray.routing.Route
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class JsonStringTest extends FreeSpec with ScalatestRouteTest {

  private val testString = """{"Ä": "a"}"""
  private val jsonString = JsonString(testString)

  "Marshal as application/json" in {
    val entity = HttpEntity(`application/json`, testString.getBytes(UTF_8))
    assert(marshal(jsonString) == Right(entity))
  }

  "Web service" - {
    "Simple Post" in {
      Post("/", jsonString) ~> testRoute(testString) ~> check {
        assert(status == OK)
        assert(contentType == `application/json`.withCharset(`UTF-8`))
        assert(entity.data.asString == testString)
      }
    }

    for (testContentType ← List(
      ContentType(`application/json`),
      `application/json` withCharset `UTF-8`))
    {
      s"$testContentType" in {
        val xmlBytes = testString.getBytes(UTF_8)
        val entity = HttpEntity(testContentType, xmlBytes)
        addHeader(Accept(testContentType.mediaType)).apply(Post("/", entity)) ~>
          testRoute(testString) ~>
          check {
            assert(status == OK)
            assert(contentType.mediaType == testContentType.mediaType)
            assert(Set(testContentType.charset, `UTF-8`) contains contentType.charset)  // The marshaller may enforce UTF-8
            assert(entity.asString(`UTF-8`) == testString)
          }
      }
    }
  }
}

private object JsonStringTest {
  import org.scalatest.Assertions._

  private def testRoute(expected: String): Route =
    entity(as[JsonString]) { xmlString ⇒
      assert(xmlString.string == expected)
      complete(xmlString)
    }
}
