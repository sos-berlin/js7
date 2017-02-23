package com.sos.scheduler.engine.common.sprayutils.web.session

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.data.session.SessionToken
import org.scalatest.FreeSpec
import spray.http.StatusCodes.{Forbidden, Unauthorized}
import spray.routing.Directives._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class SessionRegisterTest extends FreeSpec with ScalatestRouteTest {

  private val sessionRegister = new SessionRegister[String]
  private val unknownSessionToken = SessionToken(SecretString("UNKNOWN"))

  "Remove unknown SessionToken" in {
    sessionRegister.remove(unknownSessionToken)
  }

  "contains" in {
    assert(!sessionRegister.contains(unknownSessionToken))
  }

  "login" in {
    val sessionToken = sessionRegister.add("A")
    assert(sessionRegister contains sessionToken)
    sessionRegister.remove(sessionToken)
    assert(!sessionRegister.contains(sessionToken))
    sessionRegister.remove(sessionToken)
  }

  "sessionOption directive" - {
    val route = path("test") {
      sessionRegister.directives.session { session ⇒
        complete(session)
      }
    }

    "No session header" in {
      Get("/test") ~> route ~> check {
        assert(!handled)
      }
    }

    "Unknown session header is rejected" in {
      Get("/test") ~> addHeader(SessionToken.HeaderName, "UNKNOWN") ~> route ~> check {
        assert(status == Unauthorized || status == Forbidden)
      }
    }

    "Known session header" in {
      val sessionToken = sessionRegister.add("TEST-SESSION")
      Get("/test") ~> addHeader(SessionToken.HeaderName, sessionToken.secret.string) ~> route ~> check {
        assert(responseAs[String] == s"TEST-SESSION")
      }
    }
  }
}
