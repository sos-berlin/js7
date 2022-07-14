package js7.common.akkahttp.web.auth

import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.{OK, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, StatusCode, Uri}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.testkit.TestDuration
import io.circe.Json
import js7.base.auth.Permission.toStringToPermission
import js7.base.auth.User.UserDoesNotHavePermissionProblem
import js7.base.auth.{GetPermission, HashedPassword, Permission, SimpleUser, SuperPermission, User, UserAndPassword, UserId, ValidUserPermission}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Problem
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.web.auth.GateKeeper.{GetIsPublic, IsPublic, LoopbackIsPublic}
import js7.common.akkahttp.web.auth.GateKeeperTest.*
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.auth.IdToUser
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.*
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final class GateKeeperTest extends AnyFreeSpec with ScalatestRouteTest
{
  coupleScribeWithSlf4j()

  import GateKeeperTest.ImplicitGateKeeper

  private implicit val routeTestTimeout = RouteTestTimeout(10.seconds)
  private implicit def toSet(p: Permission): Set[Permission] = Set(p)

  private val defaultConf = GateKeeper.Configuration(
    realm = "REALM",
    invalidAuthenticationDelay = 100.millis,
    idToUser = IdToUser.fromConfig(config"""
      js7.auth.users {
         USER {
           password = "plain:PASSWORD"
         }
      }""",
      SimpleUser.apply,
      toStringToPermission(List(TestPermission))),
    distinguishedNameToIdsOrUser = Map.empty)

  // Anonymous with added permissions due isPublic or loopbackIsPublic
  private val publicAnonymous = defaultConf.idToUser(UserId.Anonymous).get.copy(grantedPermissions = Set(SuperPermission))

  private val GET = HttpRequest(HttpMethods.GET, Uri("URI"))
  private val POST = HttpRequest(HttpMethods.POST, Uri("URI"))

  "ifPublic (32 combinations)" - {
    val any = false :: true :: Nil
    val anyMethod = HttpMethods.GET :: HttpMethods.POST :: Nil

    "defaultConf doesn't allow public access" in {
      for (isLoopback <- any; method <- anyMethod) {
        assert(newGateKeeper(defaultConf, isLoopback = isLoopback).ifPublic(method) == None)
      }
    }

    "public=true allows public access in all cases" in {
      for (loopbackIsPublic <- any; isLoopback <- any; getIsPublic <- any; method <- anyMethod) {
        val conf = defaultConf.copy(isPublic = true, loopbackIsPublic = loopbackIsPublic, getIsPublic = getIsPublic)
        assert(newGateKeeper(conf, isLoopback = isLoopback).ifPublic(method) == Some(IsPublic))
      }
    }

    "remaining cases" in {
      assert(newGateKeeper(defaultConf.copy(getIsPublic = true)).ifPublic(HttpMethods.GET ) == Some(GetIsPublic))
      assert(newGateKeeper(defaultConf.copy(getIsPublic = true)).ifPublic(HttpMethods.POST) == None)

      assert(newGateKeeper(defaultConf.copy(getIsPublic = true), isLoopback = true ).ifPublic(HttpMethods.GET ) == Some(GetIsPublic))
      assert(newGateKeeper(defaultConf.copy(getIsPublic = true), isLoopback = true ).ifPublic(HttpMethods.POST) == None)

      assert(newGateKeeper(defaultConf.copy(loopbackIsPublic = true)).ifPublic(HttpMethods.GET) == None)
      assert(newGateKeeper(defaultConf.copy(loopbackIsPublic = true)).ifPublic(HttpMethods.POST) == None)

      assert(newGateKeeper(defaultConf.copy(loopbackIsPublic = true), isLoopback = true).ifPublic(HttpMethods.GET ) == Some(LoopbackIsPublic))
      assert(newGateKeeper(defaultConf.copy(loopbackIsPublic = true), isLoopback = true).ifPublic(HttpMethods.POST) == Some(LoopbackIsPublic))

      assert(newGateKeeper(defaultConf.copy(loopbackIsPublic = true, getIsPublic = true)).ifPublic(HttpMethods.GET ) == Some(GetIsPublic))
      assert(newGateKeeper(defaultConf.copy(loopbackIsPublic = true, getIsPublic = true)).ifPublic(HttpMethods.POST) == None)

      assert(newGateKeeper(defaultConf.copy(loopbackIsPublic = true, getIsPublic = true), isLoopback = true ).ifPublic(HttpMethods.GET ) == Some(LoopbackIsPublic))
      assert(newGateKeeper(defaultConf.copy(loopbackIsPublic = true, getIsPublic = true), isLoopback = true ).ifPublic(HttpMethods.POST) == Some(LoopbackIsPublic))
    }
  }

  "publicAnonymous" in {
    assert(publicAnonymous.hasPermission(GetPermission))
    assert(publicAnonymous.hasPermission(TestPermission))
    assert(publicAnonymous.hasPermission(SuperPermission))
    assert(publicAnonymous.hasPermission(ValidUserPermission))
  }

  "allowedUser" - {
    "isPublic" in {
      // user is authorized, allowUser adds all permissions to the user
      val gateKeeper = newGateKeeper(defaultConf.copy(isPublic = true), isLoopback = false)
      assert(gateKeeper.allowedUser(gateKeeper.anonymousUser, GET, Set.empty)            == Right(publicAnonymous))
      assert(gateKeeper.allowedUser(gateKeeper.anonymousUser, GET, ValidUserPermission)  == Right(publicAnonymous))
      assert(gateKeeper.allowedUser(gateKeeper.anonymousUser, GET, TestPermission)       == Right(publicAnonymous))
      assert(gateKeeper.allowedUser(gateKeeper.anonymousUser, POST, Set.empty)           == Right(publicAnonymous))
      assert(gateKeeper.allowedUser(gateKeeper.anonymousUser, POST, ValidUserPermission) == Right(publicAnonymous))
      assert(gateKeeper.allowedUser(gateKeeper.anonymousUser, POST, TestPermission)      == Right(publicAnonymous))
      assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)            == Right(EmpoweredTestUser))
      assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)  == Right(EmpoweredTestUser))
      assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)       == Right(EmpoweredTestUser))
      assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)           == Right(EmpoweredTestUser))
      assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(EmpoweredTestUser))
      assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)      == Right(EmpoweredTestUser))
    }

    "!getIsPublic" - {
      "!loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = false), isLoopback = false)
          val anon = gateKeeper.anonymousUser

          assert(gateKeeper.allowedUser(anon, GET, Set.empty)               == Right(anon))
          assert(gateKeeper.allowedUser(anon, GET, ValidUserPermission)     == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, GET, TestPermission)          == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(anon, POST, Set.empty)              == onlyGetPermittedForAnonymous)
          assert(gateKeeper.allowedUser(anon, POST, ValidUserPermission)    == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, POST, TestPermission)         == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)           == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission) == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)      == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, GetPermission)       == missingPermission(TestUser, GetPermission))
          assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)          == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)     == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, GetPermission)      == missingPermission(TestUser, GetPermission))
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = false), isLoopback = true)
          val anon = gateKeeper.anonymousUser

          assert(gateKeeper.allowedUser(anon, GET, Set.empty)             == Right(anon))
          assert(gateKeeper.allowedUser(anon, GET, ValidUserPermission)   == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, GET, TestPermission)        == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(anon, POST, Set.empty)            == onlyGetPermittedForAnonymous)
          assert(gateKeeper.allowedUser(anon, POST, ValidUserPermission)  == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, POST, TestPermission)       == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)            == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)  == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)       == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, GetPermission)        == missingPermission(TestUser, GetPermission))
          assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)           == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)      == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, GetPermission)       == missingPermission(TestUser, GetPermission))
        }
      }

      "loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = true), isLoopback = false)
          val anon = gateKeeper.anonymousUser

          assert(gateKeeper.allowedUser(anon, GET, Set.empty)            == Right(anon))
          assert(gateKeeper.allowedUser(anon, GET, ValidUserPermission)  == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, GET, TestPermission)       == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(anon, POST, Set.empty)           == onlyGetPermittedForAnonymous)
          assert(gateKeeper.allowedUser(anon, POST, ValidUserPermission) == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, POST, TestPermission)      == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)            == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission)  == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)       == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, GetPermission)        == missingPermission(TestUser, GetPermission))
          assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)           == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)      == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, GetPermission)       == missingPermission(TestUser, GetPermission))
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = false, loopbackIsPublic = true), isLoopback = true)
          val anon = gateKeeper.anonymousUser

          assert(gateKeeper.allowedUser(anon, GET, Set.empty)            == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, ValidUserPermission)  == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, TestPermission)       == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, Set.empty)           == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, ValidUserPermission) == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, TestPermission)      == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)           == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission) == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)      == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, GetPermission)       == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)           == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)      == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, GetPermission)       == Right(EmpoweredTestUser))
        }
      }
    }

    "getIsPublic" - {
      "!loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = false), isLoopback = false)
          val anon = gateKeeper.anonymousUser

          // Anonymous with added permissions due isPublic or loopbackIsPublic
          val PublicGetAnonymous = anon.copy(grantedPermissions = GetPermissions)

          assert(gateKeeper.allowedUser(anon, GET, Set.empty)            == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, ValidUserPermission)  == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, TestPermission)       == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, Set.empty)           == onlyGetPermittedForAnonymous)
          assert(gateKeeper.allowedUser(anon, POST, ValidUserPermission) == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, POST, TestPermission)      == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)           == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission) == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)      == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, GetPermission)       == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)           == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)      == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, GetPermission)       == missingPermission(TestUser, GetPermission))
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = false), isLoopback = true)
          val anon = gateKeeper.anonymousUser
          // Anonymous with added permissions due isPublic or loopbackIsPublic
          val PublicGetAnonymous = gateKeeper.anonymousUser.copy(grantedPermissions = GetPermissions)

          assert(gateKeeper.allowedUser(anon, GET, Set.empty)            == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, ValidUserPermission)  == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, TestPermission)       == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, Set.empty)           == onlyGetPermittedForAnonymous)
          assert(gateKeeper.allowedUser(anon, POST, ValidUserPermission) == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, POST, TestPermission)      == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)           == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission) == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)      == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, GetPermission)       == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)           == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)      == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, GetPermission)       == missingPermission(TestUser, GetPermission))
        }
      }

      "loopbackIsPublic" - {
        "!isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = true), isLoopback = false)
          val anon = gateKeeper.anonymousUser
          // Anonymous with added permissions due isPublic or loopbackIsPublic
          val PublicGetAnonymous = anon.copy(grantedPermissions = GetPermissions)

          assert(gateKeeper.allowedUser(anon, GET, Set.empty)            == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, ValidUserPermission)  == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, TestPermission)       == Right(PublicGetAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, Set.empty)           == onlyGetPermittedForAnonymous)
          assert(gateKeeper.allowedUser(anon, POST, ValidUserPermission) == missingPermission(anon, ValidUserPermission))
          assert(gateKeeper.allowedUser(anon, POST, TestPermission)      == missingPermission(anon, TestPermission))
          assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)           == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission) == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)      == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, GetPermission)       == Right(EmpoweredGetTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)           == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, TestPermission)      == Right(TestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, GetPermission)       == missingPermission(TestUser, GetPermission))
        }

        "isLoopback" in {
          val gateKeeper = newGateKeeper(defaultConf.copy(getIsPublic = true, loopbackIsPublic = true), isLoopback = true)
          val anon = gateKeeper.anonymousUser

          assert(gateKeeper.allowedUser(anon, GET, Set.empty)            == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, ValidUserPermission)  == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, GET, TestPermission)       == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, Set.empty)           == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, ValidUserPermission) == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(anon, POST, TestPermission)      == Right(publicAnonymous))
          assert(gateKeeper.allowedUser(TestUser, GET, Set.empty)           == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, ValidUserPermission) == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, TestPermission)      == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, GET, GetPermission)       == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, Set.empty)           == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, ValidUserPermission) == Right(EmpoweredTestUser))
          assert(gateKeeper.allowedUser(TestUser, POST, GetPermission)       == Right(EmpoweredTestUser))
        }
      }
    }

    def missingPermission(user: User, permission: Permission) =
      Left(UserDoesNotHavePermissionProblem(user.id, permission))

    def onlyGetPermittedForAnonymous = Left(Problem("Anonymous is permitted HTTP GET only"))
  }

  private def route(conf: GateKeeper.Configuration[SimpleUser]): Route = route(newGateKeeper(conf))

  private def route(gateKeeper: GateKeeper[SimpleUser]): Route =
    gateKeeper.preAuthenticate {
      case Left(_/*userIds*/) => fail()
      case Right(user) =>
        path("ValidUserPermission") {
          gateKeeper.authorize(user, Set(ValidUserPermission)) { user =>
            (get | post) {
              complete(user.id.string)
            }
          }
        } ~
        path("OPEN") {
          gateKeeper.authorize(user, Set.empty) { user =>  // GET is public, POST is public only with loopbackIsPublic on loopback interface
            (get | post) {
              complete(user.id.string)
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
          assert(status == OK && responseAs[String] == "Anonymous")
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
          assert(status == OK && responseAs[String] == "Anonymous")
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
          assert(status == OK && responseAs[String] == "Anonymous")
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
          assert(status == OK && responseAs[String] == "Anonymous")
        }
      }

      "POST /ValidUserPermission without authentication" in {
        Post(validUserUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(status == OK && responseAs[String] == "Anonymous")
        }
      }

      "GET /OPEN without authentication" in {
        Get(openUri) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(status == OK && responseAs[String] == "Anonymous")
        }
      }

      "POST /OPEN without authentication" in {
        Post(openUri, Json.obj()) ~> route(newGateKeeper(conf, isLoopback = true)) ~> check {
          assert(status == OK && responseAs[String] == "Anonymous")
        }
      }
    }
  }

  "getIsPublic - HTTP GET is open for Anonymous" - {
    val conf = defaultConf.copy(getIsPublic = true)

    "GET /ValidUserPermission" in {
      Get(validUserUri) ~> route(conf) ~> check {
        assert(status == OK && responseAs[String] == "Anonymous")
      }
    }

    "GET /OPEN" in {
      Get(openUri) ~> route(conf) ~> check {
        assert(status == OK && responseAs[String] == "Anonymous")
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
        assert(status == OK && responseAs[String] == "USER")
      }
    }

    "GET /ValidUserPermission without credentials is rejected" in {
      Get(validUserUri) ~> route(defaultConf) ~> check {
        assertPlainStatus(Unauthorized)
      }
    }

    "GET /ValidUserPermission with invalid credentials is delayed and rejected" in {
      implicit val routeTestTimeout = RouteTestTimeout(99.seconds.dilated)
      val runningSince = now
      Get(validUserUri) ~> Authorization(BasicHttpCredentials("USER", "WRONG")) ~> route(defaultConf) ~> check {
        assert(status == Unauthorized)
      }
      assert(runningSince.elapsed >= defaultConf.invalidAuthenticationDelay)
    }
  }

  private def newGateKeeper[U <: User: User.Companion ](conf: GateKeeper.Configuration[U], isLoopback: Boolean = false) = {
    implicit val exceptionHandler: ExceptionHandler = null  // Use default ExceptionHandler, see Route.seal
    implicit val s = Scheduler.traced
    new GateKeeper(WebServerBinding.Http, conf, isLoopback = isLoopback)
  }

  /** Error message does not contain a hint. */
  private def assertPlainStatus(statusCode: StatusCode): Unit = {
    assert(status == statusCode)
    (status: @unchecked) match {
      case OK =>
      case Unauthorized =>
        assert(headers contains `WWW-Authenticate`(HttpChallenges.basic(defaultConf.realm)))
        val responseString = Unmarshaller.stringUnmarshaller.forContentTypes(`text/plain`)(response.entity).await(99.s)
        assert(responseString == "The resource requires authentication, which was not supplied with the request")  // Akka message, by reject(AuthenticationFailedRejection(CredentialsMissing, ...)
    }
  }
}

private object GateKeeperTest
{
  private case object TestPermission extends Permission
  private val GetPermissions = Set[Permission](GetPermission)

  private val TestUser = SimpleUser(UserId("USER"), HashedPassword(SecretString("PASSWORD"), _.reverse),
    Set(TestPermission))

  private val EmpoweredTestUser = TestUser.copy(grantedPermissions = TestUser.grantedPermissions + SuperPermission)
  private val EmpoweredGetTestUser = TestUser.copy(grantedPermissions = TestUser.grantedPermissions ++ GetPermissions)

  private implicit class ImplicitGateKeeper[U <: User](private val underlying: GateKeeper[U]) extends AnyVal {
    def anonymousUser = underlying.authenticateUser(UserAndPassword(UserId.Anonymous, SecretString(""))).get
  }
}
