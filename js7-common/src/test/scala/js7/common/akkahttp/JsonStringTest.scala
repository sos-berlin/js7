package js7.common.akkahttp

import akka.http.scaladsl.model.HttpCharsets.`UTF-8`
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{ContentType, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.common.akkahttp.JsonStringTest._
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class JsonStringTest extends AnyFreeSpec with ScalatestRouteTest
{
  coupleScribeWithSlf4j()

  private val testString = """{"Ä": "a"}"""
  private val jsonString = JsonString(testString)

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
  import org.scalatest.Assertions._

  private def testRoute(expected: String): Route =
    entity(as[JsonString]) { jsonString =>
      assert(jsonString.string == expected)
      complete(jsonString)
    }
}
