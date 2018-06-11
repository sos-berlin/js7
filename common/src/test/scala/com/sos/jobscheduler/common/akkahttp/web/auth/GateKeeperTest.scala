package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.{OK, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.model.{StatusCode, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Route}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.testkit.TestDuration
import com.sos.jobscheduler.base.auth.{HashedPassword, KnownUserPermission, PermissionBundle, SimpleUser, User, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeperTest._
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import io.circe.Json
import java.time.Instant.now
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class GateKeeperTest extends FreeSpec with ScalatestRouteTest {

  private val defaultConf = GateKeeper.Configuration(
    realm = "REALM",
    invalidAuthenticationDelay = 2.s,
    httpIsPublic = false,
    getIsPublic = false,
    idToUser = {   // Like master.conf and agent.conf
      case UserId("USER")      ⇒ Some(TestUser)
      case UserId("Anonymous") ⇒ Some(SimpleUser.Anonymous)
      case _ ⇒ None
    })
  private lazy val timerService = TimerService(idleTimeout = Some(1.s))

  "isAllowed" - {
    "!getIsPublic !httpIsPublic isHttps" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, httpIsPublic = false), isHttps = true)
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, KnownUserPermission))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, KnownUserPermission))
    }

    "!getIsPublic !httpIsPublic !isHttps" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, httpIsPublic = false), isHttps = false)
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, KnownUserPermission))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, KnownUserPermission))
    }

    "!getIsPublic httpIsPublic isHttps" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, httpIsPublic = true), isHttps = true)
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, KnownUserPermission))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, KnownUserPermission))
    }

    "!getIsPublic httpIsPublic !isHttps" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, httpIsPublic = true), isHttps = false)
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, KnownUserPermission))
    }

    "getIsPublic !httpIsPublic isHttps" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, httpIsPublic = false), isHttps = true)
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, KnownUserPermission))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, KnownUserPermission))
    }

    "getIsPublic !httpIsPublic !isHttps" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, httpIsPublic = false), isHttps = false)
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, KnownUserPermission))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, KnownUserPermission))
    }

    "getIsPublic httpIsPublic isHttps" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, httpIsPublic = true), isHttps = true)
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, KnownUserPermission))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, KnownUserPermission))
    }

    "getIsPublic httpIsPublic !isHttps" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, httpIsPublic = true), isHttps = false)
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, KnownUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, KnownUserPermission))
    }
  }

  private def route(conf: GateKeeper.Configuration[SimpleUser]): Route = route(newGateKeeper(conf))

  private def route(gateKeeper: GateKeeper[SimpleUser]): Route =
    gateKeeper.authenticate { user ⇒
      path("CLOSED") {
        gateKeeper.authorize(user, PermissionBundle(Set(KnownUserPermission))) {
          (get | post) {
            complete(user.id.toString)
          }
        }
      } ~
      path("OPEN") {
        gateKeeper.authorize(user, PermissionBundle.empty) {
          (get | post) {
            complete(user.id.toString)
          }
        }
      }
    }

  private val closedUri = Uri("/CLOSED")
  private val openUri = Uri("/OPEN")

  "httpIsPublic" - {
    val conf = defaultConf.copy(httpIsPublic = true)

    "Request via secure HTTP" - {
      "GET /CLOSED without authentication is rejected" in {
        Get(closedUri) ~> route(newGateKeeper(conf, isHttps = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /CLOSED JSON without authentication is rejected" in {
        Post(closedUri, Json.obj()) ~> route(newGateKeeper(conf, isHttps = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "GET /OPEN without authentication is rejected" in {
        Get(openUri) ~> route(newGateKeeper(conf, isHttps = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /OPEN JSON without authentication is rejected" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(conf, isHttps = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }
    }

    "Request via HTTP (!isHttps)" - {
      "GET /CLOSED without authentication is rejected" in {
        Get(closedUri) ~> route(newGateKeeper(conf, isHttps = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /CLOSED JSON without authentication is rejected" in {
        Post(closedUri, Json.obj()) ~> route(newGateKeeper(conf, isHttps = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "GET /OPEN without authentication is accepted" in {
        Get(openUri) ~> route(newGateKeeper(conf, isHttps = false)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN JSON without authentication is accepted" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(conf, isHttps = false)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }
    }
  }

  "getIsPublic - HTTP GET is open for everybody" - {
    val conf = defaultConf.copy(getIsPublic = true)

    "GET /CLOSED is rejected" in {
      Get(closedUri) ~> route(conf) ~> check {
        assertPlainStatus(Unauthorized)
      }
    }

    "GET /OPEN" in {
      Get(openUri) ~> route(conf) ~> check {
        assert(responseAs[String] == "Anonymous")
      }
    }

    "POST JSON" in {
      Post(closedUri, Json.obj()) ~> route(conf) ~> check {
        assertPlainStatus(Unauthorized)
      }
    }
  }

  "User ID and password" - {
    "Access with valid credentials" in {
      Get(closedUri) ~> Authorization(BasicHttpCredentials("USER", "PASSWORD")) ~> route(defaultConf) ~> check {
        assert(responseAs[String] == "USER")
      }
    }

    "No access with missing credentials" in {
      Get(closedUri) ~> route(defaultConf) ~> check {
        assertPlainStatus(Unauthorized)
      }
    }

    "No access with invalid credentials" in {
      implicit def default(implicit system: ActorSystem) =
        RouteTestTimeout((2 * defaultConf.invalidAuthenticationDelay).toFiniteDuration.dilated)

      val t = now
      Get(closedUri) ~> Authorization(BasicHttpCredentials("USER", "WRONG")) ~> route(defaultConf) ~> check {
        assert(status == Unauthorized)
      }
      assert(now - t >= defaultConf.invalidAuthenticationDelay - 50.ms)  // Allow for timer rounding
    }
  }

  "Access token" - (if (false) {  // TODO Not implemented - do we need this?

    "Access with valid access token" in {
      Get(closedUri) ~> Authorization(BasicHttpCredentials("", "GRETA-TOKEN")) ~> route(defaultConf) ~> check {
        assert(status == OK)
        assert(responseAs[String] == "SimpleUser(GRETA)")
      }
    }

    "No access with missing credentials" in {
      Get(closedUri) ~> route(defaultConf) ~> check {
        assert(!handled)
        assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
      }
    }

    "No access with invalid credentials" in {
      implicit def default(implicit system: ActorSystem) =
        RouteTestTimeout((2 * defaultConf.invalidAuthenticationDelay).toFiniteDuration.dilated)

      val t = now
      Get(closedUri) ~> Authorization(BasicHttpCredentials("", "WRONG")) ~> route(defaultConf) ~> check {
        assert(status == Unauthorized)
      }
      assert(now - t >= defaultConf.invalidAuthenticationDelay - 50.ms)  // Allow for timer rounding
    }
  })

  private def newGateKeeper[U <: User](conf: GateKeeper.Configuration[U], isHttps: Boolean = true)(implicit ec: ExecutionContext) =
    new GateKeeper(conf, timerService, isHttps = isHttps)

  /** Error message does not contain a hint. */
  private def assertPlainStatus(statusCode: StatusCode): Unit = {
    assert(status == statusCode)
    val responseString = Unmarshaller.stringUnmarshaller.forContentTypes(`text/plain`)(response.entity).await(99.s)
    status match {
      case Unauthorized ⇒
        assert(headers contains `WWW-Authenticate`(HttpChallenges.basic(defaultConf.realm)))
        assert(responseString == "The resource requires authentication, which was not supplied with the request")  // Akka message, by reject(AuthenticationFailedRejection(CredentialsMissing, ...)
      case _ ⇒
        assert(responseString == statusCode.defaultMessage)
    }
  }
}

private object GateKeeperTest {
  private val TestUser = SimpleUser(UserId("USER"), HashedPassword(SecretString("DROWSSAP"), _.reverse))
}
