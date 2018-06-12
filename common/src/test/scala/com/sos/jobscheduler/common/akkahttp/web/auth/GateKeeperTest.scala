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
import com.sos.jobscheduler.base.auth.{HashedPassword, PermissionBundle, SimpleUser, User, UserId, ValidUserPermission}
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
    idToUser = {   // Like master.conf and agent.conf
      case UserId("USER")      ⇒ Some(TestUser)
      case UserId("Anonymous") ⇒ Some(SimpleUser.Anonymous)
      case _ ⇒ None
    })
  private lazy val timerService = TimerService(idleTimeout = Some(1.s))

  "isAllowed" - {
    "isPublic" in {
      val gateKeeper = newGateKeeper(defaultConf.copy(isPublic = true), isLoopback = false)
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
      assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
      assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
      assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
    }

    "!getIsPublic" - {
      "!loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = false), isLoopback = false)
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = false), isLoopback = true)
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
        }
      }

      "loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = true), isLoopback = false)
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = true), isLoopback = true)
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
        }
      }
    }

    "getIsPublic" - {
      "!loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = false), isLoopback = false)
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = false), isLoopback = true)
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
        }
      }

      "loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = true), isLoopback = false)
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
          assert(!gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = true), isLoopback = true)
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(SimpleUser.Anonymous, POST, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, GET, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, GET, ValidUserPermission))
          assert(gateKeeper.isAllowed(TestUser, POST, PermissionBundle.empty))
          assert(gateKeeper.isAllowed(TestUser, POST, ValidUserPermission))
        }
      }
    }
  }

  private def route(conf: GateKeeper.Configuration[SimpleUser]): Route = route(newGateKeeper(conf))

  private def route(gateKeeper: GateKeeper[SimpleUser]): Route =
    gateKeeper.authenticate { user ⇒
      path("CLOSED") {
        gateKeeper.authorize(user, PermissionBundle(Set(ValidUserPermission))) {
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

  "!loopbackIsPublic" - {
    "Request via HTTP (!isLoopback)" - {
      "GET /CLOSED without authentication is rejected" in {
        Get(closedUri) ~> route(newGateKeeper(defaultConf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /CLOSED JSON without authentication is rejected" in {
        Post(closedUri, Json.obj()) ~> route(newGateKeeper(defaultConf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "GET /OPEN without authentication is allowed" in {
        Get(openUri) ~> route(newGateKeeper(defaultConf, isLoopback = false)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN JSON without authentication rejected" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(defaultConf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }
    }

    "Request via loopback" - {
      "GET /CLOSED without authentication is rejected" in {
        Get(closedUri) ~> route(newGateKeeper(defaultConf, isLoopback = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /CLOSED JSON without authentication is rejected" in {
        Post(closedUri, Json.obj()) ~> route(newGateKeeper(defaultConf, isLoopback = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "GET /OPEN without authentication is allowed" in {
        Get(openUri) ~> route(newGateKeeper(defaultConf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN JSON without authentication is rejected" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(defaultConf, isLoopback = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }
    }
  }

  "loopbackIsPublic" - {
    val conf = defaultConf.copy(loopbackIsPublic = true)

    "Request via HTTP (!isLoopback)" - {
      "GET /CLOSED without authentication is rejected" in {
        Get(closedUri) ~> route(newGateKeeper(conf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /CLOSED JSON without authentication is rejected" in {
        Post(closedUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "GET /OPEN without authentication is allowed" in {
        Get(openUri) ~> route(newGateKeeper(conf, isLoopback = false)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN JSON without authentication rejected" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }
    }

    "Request via loopback" - {
      "GET /CLOSED without authentication is allowed" in {
        Get(closedUri) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /CLOSED JSON without authentication is allowed" in {
        Post(closedUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "GET /OPEN without authentication is allowed" in {
        Get(openUri) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN JSON without authentication is allowed" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }
    }
  }

  "getIsPublic - HTTP GET is open for Anonymous" - {
    val conf = defaultConf.copy(getIsPublic = true)

    "GET /CLOSED is allowed" in {
      Get(closedUri) ~> route(conf) ~> check {
        assert(responseAs[String] == "Anonymous")
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

  private def newGateKeeper[U <: User](conf: GateKeeper.Configuration[U], isLoopback: Boolean = false)(implicit ec: ExecutionContext) =
    new GateKeeper(conf, timerService, isLoopback = isLoopback)

  /** Error message does not contain a hint. */
  private def assertPlainStatus(statusCode: StatusCode): Unit = {
    assert(status == statusCode)
    status match {
      case OK ⇒
      case Unauthorized ⇒
        assert(headers contains `WWW-Authenticate`(HttpChallenges.basic(defaultConf.realm)))
        val responseString = Unmarshaller.stringUnmarshaller.forContentTypes(`text/plain`)(response.entity).await(99.s)
        assert(responseString == "The resource requires authentication, which was not supplied with the request")  // Akka message, by reject(AuthenticationFailedRejection(CredentialsMissing, ...)
    }
  }
}

private object GateKeeperTest {
  private val TestUser = SimpleUser(UserId("USER"), HashedPassword(SecretString("DROWSSAP"), _.reverse))
}
