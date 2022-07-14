package js7.common.akkahttp.web.auth

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{Forbidden, OK}
import akka.http.scaladsl.model.{HttpEntity, Uri}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.common.akkahttp.web.auth.CSRF.forbidCSRF
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CSRFTest extends AnyFreeSpec with ScalatestRouteTest {

  private val route: Route =
    forbidCSRF {
      path("TEST") {
        (get | post) {
          complete(OK)
        }
      }
    }

  private val uri = Uri("/TEST")

  "GET is allowed" in {
    Get(uri) ~> route ~> check {
      assert(status == OK)
    }
  }

  "POST application/json allowed" in {
    Post(uri, HttpEntity(`application/json`, "{}")) ~> route ~> check {
      assert(status == OK)
    }
  }

  "POST text/plain is forbidden" in {
    Post(uri, "STRING") ~> route ~> check {
      assert(status == Forbidden)
      assert(responseAs[String] == Forbidden.defaultMessage)
    }
  }
}
