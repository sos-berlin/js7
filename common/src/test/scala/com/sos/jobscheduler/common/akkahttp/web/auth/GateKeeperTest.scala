package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.{OK, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, StatusCode, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.testkit.TestDuration
import com.sos.jobscheduler.base.auth.{HashedPassword, Permission, PermissionBundle, SimpleUser, User, UserId, ValidUserPermission}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeperTest._
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.time.ScalaTime._
import io.circe.Json
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class GateKeeperTest extends FreeSpec with ScalatestRouteTest
{
  private implicit val routeTestTimeout = RouteTestTimeout(10.seconds)

  private val defaultConf = GateKeeper.Configuration(
    realm = "REALM",
    invalidAuthenticationDelay = 100.millis,
    idToUser = {   // Like master.conf and agent.conf
      case UserId("USER")      ⇒ Some(TestUser)
      case UserId("Anonymous") ⇒ Some(SimpleUser.Anonymous)
      case _ ⇒ None
    },
    publicPermissions = PermissionBundle(AllPermissions))
  private val GET = HttpRequest(HttpMethods.GET, Uri("URI"))
  private val POST = HttpRequest(HttpMethods.POST, Uri("URI"))

  "allowedUser" - {
    "isPublic" in {
      // user is authorized, allowUser adds all permissions to the user
      val gateKeeper = newGateKeeper(defaultConf.copy(isPublic = true), isLoopback = false)
      assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(PublicAnonymous))
      assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == Some(PublicAnonymous))
      assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == Some(PublicAnonymous))
      assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == Some(PublicAnonymous))
      assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == Some(PublicAnonymous))
      assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == Some(PublicAnonymous))
      assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(PublicTestUser))
      assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(PublicTestUser))
      assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(PublicTestUser))
      assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(PublicTestUser))
      assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(PublicTestUser))
      assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)         == Some(PublicTestUser))
    }

    "!getIsPublic" - {
      "!loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = false), isLoopback = false)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == None)
          assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, Test2Permission)         == None)
          assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Test2Permission)        == None)
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = false), isLoopback = true)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == None)
          assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, Test2Permission)         == None)
          assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Test2Permission)        == None)
        }
      }

      "loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = true), isLoopback = false)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == None)
          assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, Test2Permission)         == None)
          assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Test2Permission)        == None)
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = true), isLoopback = true)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, Test2Permission)         == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)         == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Test2Permission)        == Some(PublicTestUser))
        }
      }
    }

    "getIsPublic" - {
      "!loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = false), isLoopback = false)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == None)
          assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, Test2Permission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Test2Permission)        == None)
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = false), isLoopback = true)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == None)
          assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, Test2Permission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Test2Permission)        == None)
        }
      }

      "loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = true), isLoopback = false)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == Some(SimpleUser.Anonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == None)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == None)
          assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, Test2Permission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)         == Some(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Test2Permission)        == None)
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = true), isLoopback = true)
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, PermissionBundle.empty)  == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, ValidUserPermission)     == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, GET, TestPermission)          == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, PermissionBundle.empty) == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, ValidUserPermission)    == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(SimpleUser.Anonymous, POST, TestPermission)         == Some(PublicAnonymous))
          assert(gateKeeper.allowedUser(TestUser, GET, PermissionBundle.empty)  == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)     == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)          == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, Test2Permission)         == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, PermissionBundle.empty) == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission)    == Some(PublicTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Test2Permission)        == Some(PublicTestUser))
        }
      }
    }
  }

  private def route(conf: GateKeeper.Configuration[SimpleUser]): Route = route(newGateKeeper(conf))

  private def route(gateKeeper: GateKeeper[SimpleUser]): Route =
    gateKeeper.authenticate { user ⇒
      path("ValidUserPermission") {
        gateKeeper.authorize(user, PermissionBundle(Set(ValidUserPermission))) { user ⇒
          (get | post) {
            complete(user.id.toString)
          }
        }
      } ~
      path("OPEN") {
        gateKeeper.authorize(user, PermissionBundle.empty) { user ⇒  // GET is public, POST is public only with loopbackIsPublic on loopback interface
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

    "GET /ValidUserPermission with invalid credentials is delayed and rejected" in {
      implicit val routeTestTimeout = RouteTestTimeout(9.seconds.dilated)
      val t = now
      Get(validUserUri) ~> Authorization(BasicHttpCredentials("USER", "WRONG")) ~> route(defaultConf) ~> check {
        assert(status == Unauthorized)
      }
      assert(now - t >= defaultConf.invalidAuthenticationDelay)
    }
  }

  private def newGateKeeper[U <: User: User.Companion ](conf: GateKeeper.Configuration[U], isLoopback: Boolean = false) = {
    implicit val exceptionHandler: ExceptionHandler = null  // Use default ExceptionHandler, see Route.seal
    implicit val s = Scheduler.global
    new GateKeeper(conf, isLoopback = isLoopback)
  }

  /** Error message does not contain a hint. */
  private def assertPlainStatus(statusCode: StatusCode): Unit = {
    assert(status == statusCode)
    (status: @unchecked) match {
      case OK ⇒
      case Unauthorized ⇒
        assert(headers contains `WWW-Authenticate`(HttpChallenges.basic(defaultConf.realm)))
        val responseString = Unmarshaller.stringUnmarshaller.forContentTypes(`text/plain`)(response.entity).await(99.s)
        assert(responseString == "The resource requires authentication, which was not supplied with the request")  // Akka message, by reject(AuthenticationFailedRejection(CredentialsMissing, ...)
    }
  }
}

private object GateKeeperTest {
  private case object TestPermission extends Permission
  private case object Test2Permission extends Permission
  private val AllPermissions = Set[Permission](TestPermission, Test2Permission)

  private val TestUser = SimpleUser(UserId("USER"), HashedPassword(SecretString("DROWSSAP"), _.reverse),
    PermissionBundle(Set(TestPermission)))

  private val PublicTestUser = TestUser.copy(grantedPermissions = PermissionBundle(AllPermissions + ValidUserPermission))

  // Anonymous with added permissions due isPublic or loopbackIsPublic
  private val PublicAnonymous = SimpleUser.Anonymous.copy(grantedPermissions = PermissionBundle(AllPermissions))
}
