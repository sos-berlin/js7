package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.{OK, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.model.{StatusCode, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
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
    invalidAuthenticationDelay = 100.ms,
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
      path("ValidUserPermission") {
        gateKeeper.authorize(user, PermissionBundle(Set(ValidUserPermission))) {
          (get | post) {
            complete(user.id.toString)
          }
        }
      } ~
      path("OPEN") {
        gateKeeper.authorize(user, PermissionBundle.empty) {  // GET is public, POST is public only with loopbackIsPublic on loopback interface
          (get | post) {
            complete(user.id.toString)
          }
        }
      }
    }

  private val validUserUri = Uri("/ValidUserPermission")
  private val openUri = Uri("/OPEN")

  "!loopbackIsPublic" - {
    "Request via HTTP (!isLoopback)" - {
      "GET /ValidUserPermission without authentication is rejected" in {
        Get(validUserUri) ~> route(newGateKeeper(defaultConf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /ValidUserPermission without authentication is rejected" in {
        Post(validUserUri, Json.obj()) ~> route(newGateKeeper(defaultConf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "GET /OPEN without authentication" in {
        Get(openUri) ~> route(newGateKeeper(defaultConf, isLoopback = false)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN without authentication rejected" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(defaultConf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }
    }

    "Request via loopback" - {
      "GET /ValidUserPermission without authentication is rejected" in {
        Get(validUserUri) ~> route(newGateKeeper(defaultConf, isLoopback = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /ValidUserPermission without authentication is rejected" in {
        Post(validUserUri, Json.obj()) ~> route(newGateKeeper(defaultConf, isLoopback = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "GET /OPEN without authentication" in {
        Get(openUri) ~> route(newGateKeeper(defaultConf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN without authentication is rejected" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(defaultConf, isLoopback = true)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }
    }
  }

  "loopbackIsPublic" - {
    val conf = defaultConf.copy(loopbackIsPublic = true)

    "Request via HTTP (!isLoopback)" - {
      "GET /ValidUserPermission without authentication is rejected" in {
        Get(validUserUri) ~> route(newGateKeeper(conf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "POST /ValidUserPermission without authentication is rejected" in {
        Post(validUserUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }

      "GET /OPEN without authentication" in {
        Get(openUri) ~> route(newGateKeeper(conf, isLoopback = false)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN without authentication rejected" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = false)) ~> check {
          assertPlainStatus(Unauthorized)
        }
      }
    }

    "Request via loopback" - {
      "GET /ValidUserPermission without authentication" in {
        Get(validUserUri) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /ValidUserPermission without authentication" in {
        Post(validUserUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "GET /OPEN without authentication" in {
        Get(openUri) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN without authentication" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(responseAs[String] == "Anonymous")
        }
      }
    }
  }

  "getIsPublic - HTTP GET is open for Anonymous" - {
    val conf = defaultConf.copy(getIsPublic = true)

    "GET /ValidUserPermission" in {
      Get(validUserUri) ~> route(conf) ~> check {
        assert(responseAs[String] == "Anonymous")
      }
    }

    "GET /OPEN" in {
      Get(openUri) ~> route(conf) ~> check {
        assert(responseAs[String] == "Anonymous")
      }
    }

    "POST /ValidUserPermission is rejected" in {
      Post(validUserUri, Json.obj()) ~> route(conf) ~> check {
        assertPlainStatus(Unauthorized)
      }
    }
  }

  "User ID and password" - {
    "GET /ValidUserPermission with valid credentials" in {
      Get(validUserUri) ~> Authorization(BasicHttpCredentials("USER", "PASSWORD")) ~> route(defaultConf) ~> check {
        assert(responseAs[String] == "USER")
      }
    }

    "GET /ValidUserPermission without credentials is rejected" in {
      Get(validUserUri) ~> route(defaultConf) ~> check {
        assertPlainStatus(Unauthorized)
      }
    }

    "GET /ValidUserPermission with invalid credentials is rejected" in {
      implicit def default(implicit system: ActorSystem) =
        RouteTestTimeout((2 * defaultConf.invalidAuthenticationDelay).toFiniteDuration.dilated)

      val t = now
      Get(validUserUri) ~> Authorization(BasicHttpCredentials("USER", "WRONG")) ~> route(defaultConf) ~> check {
        assert(status == Unauthorized)
      }
      assert(now - t >= defaultConf.invalidAuthenticationDelay)
    }
  }

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
