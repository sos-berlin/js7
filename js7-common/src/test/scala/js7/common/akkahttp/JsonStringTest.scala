package js7.common.akkahttp

import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.MediaTypes.*
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{ContentType, HttpEntity}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.common.akkahttp.JsonStringTest.*

/**
 * @author Joacim Zschimmer
 */
final class JsonStringTest extends OurTestSuite with ScalatestRouteTest
{
  private val testString = """{"Ä": "a"}"""
  private val jsonString = JsonString(testString)

  override def testConfig = config"akka.loglevel = warning"
    .withFallback(super.testConfig)

  "Marshal as application/json" in {
    val entity = HttpEntity(`application/json`, testString.getBytes(UTF_8))
    assert(marshal(jsonString) == entity)
  }

  "Web service" - {
    "SimpleSession Post" in {
      Post("/", jsonString) ~> testRoute(testString) ~> check {
        assert(status == OK)
        assert(contentType == ContentType(`application/json`))
        assert(entityAs[String] == testString)
      }
    }

    "application/json" in {
      val xmlBytes = testString.getBytes(UTF_8)
      val entity = HttpEntity(`application/json`, xmlBytes)
      addHeader(Accept(`application/json`)).apply(Post("/", entity)) ~>
        testRoute(testString) ~>
        check {
          assert(status == OK)
          assert(contentType.mediaType == `application/json`)
          assert(Set(`application/json`.charset, `UTF-8`) contains contentType.charsetOption.get)  // The marshaller may enforce UTF-8
          assert(entityAs[String] == testString)
        }
    }
  }
}

private object JsonStringTest {
  import org.scalatest.Assertions.*

  private def testRoute(expected: String): Route =
    entity(as[JsonString]) { jsonString =>
      assert(jsonString.string == expected)
      complete(jsonString)
    }
}
