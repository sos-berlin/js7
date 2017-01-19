package com.sos.scheduler.engine.common.sprayutils.web.auth

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpCharsets.`UTF-8`
import spray.http.MediaTypes.`application/json`
import spray.http.StatusCodes.{Forbidden, OK}
import spray.http.Uri
import spray.httpx.marshalling.BasicMarshallers.stringMarshaller
import spray.routing.Directives._
import spray.routing._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class CSRFTest extends FreeSpec with ScalatestRouteTest {

  private val csrf = new CSRF(CSRF.Configuration.Default)

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
    Post(uri, "{}")(stringMarshaller(`application/json` withCharset `UTF-8`)) ~> route ~> check {
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
