package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes.{Forbidden, OK, Unauthorized}
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Route}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.JsObjectMarshallers.JsObjectMarshaller
import com.sos.jobscheduler.common.auth.{HashedPassword, UserId}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import java.time.Instant.now
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext
import spray.json.JsObject

/**
  * @author Joacim Zschimmer
  */
final class GateKeeperTest extends FreeSpec with ScalatestRouteTest {

  private val defaultConf = GateKeeper.Configuration(
    realm = "REALM",
    invalidAuthenticationDelay = 2.s,
    userIdToHashedPassword = () ⇒ {
      case UserId("USER") ⇒ Some(HashedPassword(SecretString("DROWSSAP"), _.reverse))
      case _ ⇒ None
    })
    //provideAccessTokenValidator = () ⇒ Map(SecretString("GRETA-TOKEN") → UserId("GRETA")))
  private lazy val timerService = TimerService(idleTimeout = Some(1.s))

  private def route(conf: GateKeeper.Configuration): Route = route(newGateKeeper(conf))

  private def route(gateKeeper: GateKeeper): Route =
    gateKeeper.restrict.apply { user ⇒
      path("TEST") {
        (get | post) {
          complete(user.toString)
        }
      }
    }

  private val uri = Uri("/TEST")

  "httpIsPublic" - {
    val conf = defaultConf.copy(httpIsPublic = true)

    "Request via secured HTTP" - {
      "GET without authentication is rejected" in {
        Get(uri) ~> route(newGateKeeper(conf, isUnsecuredHttp = false)) ~> check {
          assert(!handled)
          assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
        }
      }

      "POST JSON without authentication is rejected" in {
        Post(uri, JsObject()) ~> route(newGateKeeper(conf, isUnsecuredHttp = false)) ~> check {
          assert(!handled)
          assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
        }
      }

      addPostTextPlainText(conf)
    }

    "Request via inSecuredHttp" - {
      "GET without authentication is accepted" in {
        Get(uri) ~> route(newGateKeeper(conf, isUnsecuredHttp = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST JSON without authentication is accepted" in {
        Post(uri, JsObject()) ~> route(newGateKeeper(conf, isUnsecuredHttp = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      addPostTextPlainText(conf)
    }
  }

  "getIsPublic - HTTP GET is open for everybody" - {
    val conf = defaultConf.copy(getIsPublic = true)

    "GET" in {
      Get(uri) ~> route(conf) ~> check {
        assert(responseAs[String] == "Anonymous")
      }
    }

    "POST JSON" in {
      Post(uri, JsObject()) ~> route(conf) ~> check {
        assert(!handled)
        assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
      }
    }

    addPostTextPlainText(conf)
  }

  "User ID and password" - {
    "Access with valid credentials" in {
      Get(uri) ~> Authorization(BasicHttpCredentials("USER", "PASSWORD")) ~> route(defaultConf) ~> check {
        assert(responseAs[String] == "SimpleUser(USER)")
      }
    }

    "No access with missing credentials" in {
      Get(uri) ~> route(defaultConf) ~> check {
        assert(!handled)
        assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
      }
    }

    "No access with invalid credentials" in {
      implicit def default(implicit system: ActorSystem) =
        RouteTestTimeout((2 * defaultConf.invalidAuthenticationDelay).toFiniteDuration.dilated)

      val t = now
      Get(uri) ~> Authorization(BasicHttpCredentials("USER", "WRONG")) ~> route(defaultConf) ~> check {
        assert(status == Unauthorized)
      }
      assert(now - t >= defaultConf.invalidAuthenticationDelay - 50.ms)  // Allow for timer rounding
    }
  }

  "Access token" - (if (false) {  // TODO Not implemented - do we need this?

    "Access with valid access token" in {
      Get(uri) ~> Authorization(BasicHttpCredentials("", "GRETA-TOKEN")) ~> route(defaultConf) ~> check {
        assert(status == OK)
        assert(responseAs[String] == "SimpleUser(GRETA)")
      }
    }

    "No access with missing credentials" in {
      Get(uri) ~> route(defaultConf) ~> check {
        assert(!handled)
        assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
      }
    }

    "No access with invalid credentials" in {
      implicit def default(implicit system: ActorSystem) =
        RouteTestTimeout((2 * defaultConf.invalidAuthenticationDelay).toFiniteDuration.dilated)

      val t = now
      Get(uri) ~> Authorization(BasicHttpCredentials("", "WRONG")) ~> route(defaultConf) ~> check {
        assert(status == Unauthorized)
      }
      assert(now - t >= defaultConf.invalidAuthenticationDelay - 50.ms)  // Allow for timer rounding
    }
  })

  private def addPostTextPlainText(conf: GateKeeper.Configuration): Unit = {
    "POST text/plain is rejected due to CSRF" in {
      Post(uri, "TEXT") ~> route(newGateKeeper(conf, isUnsecuredHttp = true)) ~> check {
        assert(status == Forbidden)
      }
    }
  }

  private def newGateKeeper(conf: GateKeeper.Configuration, isUnsecuredHttp: Boolean = false)(implicit ec: ExecutionContext) =
    new GateKeeper(conf, new CSRF(CSRF.Configuration.ForTest), timerService, isUnsecuredHttp = isUnsecuredHttp)
}
