package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{Forbidden, OK}
import akka.http.scaladsl.model.{HttpEntity, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CSRFTest extends FreeSpec with ScalatestRouteTest {

  private val csrf = new CSRF(CSRF.Configuration.ForTest)

  private val route: Route =
    csrf.rejectSomeCSRF {
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

  "POST plain/text is forbidden" in {
    Post(uri, "STRING") ~> route ~> check {
      assert(status == Forbidden)
      assert(responseAs[String] == "HTML form POST is forbidden")
    }
  }
}
