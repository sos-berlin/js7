package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.StatusCodes.{BadRequest, Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.base.auth.{SessionToken, SimpleUser, UserAndPassword, UserId, ValidUserPermission}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRouteTest._
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import monix.execution.Scheduler
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
sealed abstract class SessionRouteTest(isPublic: Boolean)
extends FreeSpec with BeforeAndAfterAll with ScalatestRouteTest with SessionRoute
{
  protected type Session = SimpleSession

  implicit protected def scheduler = Scheduler.global
  protected val config = ConfigFactory.parseString("jobscheduler.webserver.verbose-error-messages = on")
  private lazy val actorSystem = newActorSystem("SessionRouteTest")

  protected lazy val gateKeeper = new GateKeeper(
    GateKeeper.Configuration.fromConfig(
      ConfigFactory.parseString(
         s"""jobscheduler.webserver.auth {
            |  realm = "TEST REALM"
            |  invalid-authentication-delay = 100ms
            |  loopback-is-public = false
            |  get-is-public = false
            |  public = $isPublic
            |}
            |
            |jobscheduler.auth.users {
            |  A-USER = "plain:A-PASSWORD"
            |  B-USER = "plain:B-PASSWORD"
            |}
            |""".stripMargin),
      SimpleUser.apply))

  protected lazy val sessionRegister =
    SessionRegister.start[SimpleSession](system, SimpleSession.apply, SessionRegister.TestConfig)

  private implicit val routeTestTimeout = RouteTestTimeout(10.seconds)

  private val route = Route.seal(
    decodeRequest {
      pathSegment("session") {
        sessionRoute
      } ~
      path("authorizedUser") {
        get {
          authorizedUser(ValidUserPermission) { user =>
            complete(user.id.string)
          }
        }
      } ~
      path("unprotected") {
        get {
          complete("THE RESPONSE")
        }
      }
    })

  private lazy val server = new AkkaWebServer with AkkaWebServer.HasUri {
    def actorSystem = SessionRouteTest.this.actorSystem
    def scheduler = Scheduler.global
    val bindings = WebServerBinding.Http(new InetSocketAddress("127.0.0.1", findRandomFreeTcpPort())) :: Nil
    def newRoute(binding: WebServerBinding) = route
  }

  import gateKeeper.invalidAuthenticationDelay
  import server.localUri

  override def afterAll() = {
    server.close()
    actorSystem.terminate() await 99.s
    super.afterAll()
  }

  "login fails if server is unreachable" in {
    withSessionApi() { api =>
      val exception = intercept[akka.stream.StreamTcpException] {
        api.login(None) await 99.s
      }
      assert(exception.getMessage contains "java.net.ConnectException")
    }
  }

  "loginUntilReachable" - {
    "invalid authorization" in {
      withSessionApi() { api =>
        var count = 0
        def onError(t: Throwable) = {
          logger.debug(t.toStringWithCauses)
          count += 1
        }
        val t = now
        val whenLoggedIn = api.loginUntilReachable(
          Some(UserId("INVALID") -> SecretString("INVALID")),
          Iterator.continually(10.milliseconds),
          onError
        ).runToFuture
        // Akka delays 100ms, 200ms, 400ms: "Connection attempt failed. Backing off new connection attempts for at least 100 milliseconds"
        waitForCondition(9.seconds, 10.milliseconds)(count >= 3)
        assert(count >= 3)
        server.start() await 99.s
        val exception = intercept[AkkaHttpClient.HttpException] {
          whenLoggedIn await 99.s
        }
        assert(exception.status == Unauthorized)
        assert(now - t >= invalidAuthenticationDelay)
        requireAccessIsUnauthorizedOrPublic(api)
      }
    }

    "authorized" in {
      withSessionApi() { api =>
        api.loginUntilReachable(Some(AUserAndPassword), Iterator.continually(10.milliseconds), _ => ()) await 99.s
        requireAuthorizedAccess(api)
        api.logout() await 99.s
        requireAccessIsUnauthorizedOrPublic(api)
      }
    }
  }

  "Login without credentials is accepted as Anonymous but access is nevertheless unauthorized if not public" in {
    withSessionApi() { api =>
      api.login(None) await 99.s
      requireAccessIsUnauthorizedOrPublic(api)
    }
  }

  "Login without credentials and with wrong Authorization header is rejected with 401 Unauthorized and delayed" in {
    withSessionApi(Authorization(BasicHttpCredentials("A-USER", "")) :: Nil) { api =>
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login(None) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(now - t >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(api)
    }
  }

  "Login without credentials and with Anonymous Authorization header is rejected and delayed" in {
    withSessionApi(Authorization(BasicHttpCredentials(UserId.Anonymous.string, "")) :: Nil) { api =>
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login(None) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(now - t >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(api)
    }
  }

  "Login with credentials and with Authorization header is rejected" in {
    withSessionApi(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil) { api =>
      requireAuthorizedAccess(api)
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login(Some(AUserAndPassword)) await 99.s
      }
      assert(exception.status == BadRequest)
      assert(exception.dataAsString.parseJsonOrThrow.as[Problem] == Right(Problem("Both command Login and HTTP header authentication?")))
      requireAuthorizedAccess(api)
    }
  }

  "Login without credentials but with Authorization header is accepted" in {
    withSessionApi(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil) { api =>
      api.login(None) await 99.s
      assert(api.hasSession)
      requireAuthorizedAccess(api)
    }
  }

  "Login as 'Anonymous' is rejected" in {
    withSessionApi() { api =>
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login(Some(UserId.Anonymous -> SecretString(""))) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(HttpChallenges.basic("TEST REALM") :: Nil)))

      requireAccessIsUnauthorizedOrPublic(api)  // public=true allows access
    }
  }

  "Login with invalid credentials is rejected with 403 Unauthorized and delayed" in {
    withSessionApi() { api =>
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login(Some(UserId("A-USER") -> SecretString(""))) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(HttpChallenges.basic("TEST REALM") :: Nil)))
      assert(exception.dataAsString contains "Login: unknown user or invalid password")
      assert(now - t >= invalidAuthenticationDelay)

      requireAccessIsUnauthorizedOrPublic(api)  // public=true allows access
    }
  }

  "Login and Logout" in {
    withSessionApi() { api =>
      assert(!api.hasSession)

      if (isPublic) {
          // Access without Login (Session) is allowed
          api.get_[String](s"$localUri/authorizedUser") await 99.s
      } else {
        // Access without Login (Session) is rejected
        intercept[HttpException] {
          api.get_[String](s"$localUri/authorizedUser") await 99.s
        }.status shouldEqual Unauthorized
        assert(!api.hasSession)
      }

      // Login and Logout
      api.login(Some(AUserAndPassword)) await 99.s
      val Some(sessionToken) = api.sessionToken
      assert(api.hasSession)
      requireAuthorizedAccess(api)
      assert(api.hasSession)
      api.logout() await 99.s shouldEqual Completed
      assert(!api.hasSession)

      // Using old SessionToken is Unauthorized
      api.setSessionToken(sessionToken)
      val exception = requireAccessIsForbidden(api)
      assert(AkkaHttpClient.sessionMayBeLost(exception))
    }
  }

  "Logout without SessionToken is short-circuited" in {
    withSessionApi() { api =>
      assert(!api.hasSession)
      api.logout() await 99.s shouldEqual Completed
      assert(!api.hasSession)
    }
  }

  "Use of discarded SessionToken is Forbidden, clearSession" in {
    // This applies to all commands, also Login and Logout.
    // With Unauthorized or Forbidden, stateful SessionApi learns about the invalid session.
    withSessionApi() { api =>
      api.setSessionToken(SessionToken(SecretString("DISCARDED")))
      val exception = requireAccessIsForbidden(api)
      assert(AkkaHttpClient.sessionMayBeLost(exception))
      assert(api.hasSession)

      api.clearSession()
      api.login(Some(AUserAndPassword)) await 99.s
      assert(api.hasSession)
      api.logout() await 99.s
    }
  }

  "logout clears SessionToken even if invalid" in {
    withSessionApi() { api =>
      api.setSessionToken(SessionToken(SecretString("DISCARDED")))
      assert(api.hasSession)
      api.logout() await 99.s
      assert(!api.hasSession)
    }
  }

  "Second Login invalidates first Login" in {
    withSessionApi() { api =>
      api.login(Some(AUserAndPassword)) await 99.s
      val Some(firstSessionToken) = api.sessionToken
      assert(api.hasSession)
      api.login(Some(BUserAndPassword)) await 99.s
      assert(api.hasSession)

      withSessionApi() { otherClient =>
        // Using old SessionToken is Unauthorized
        otherClient.setSessionToken(firstSessionToken)
        val exception = intercept[AkkaHttpClient.HttpException] {
          otherClient.get_[String](s"$localUri/authorizedUser") await 99.s
        }
        requireAccessIsForbidden(otherClient)
        assert(AkkaHttpClient.sessionMayBeLost(exception))
      }

      api.logout() await 99.s shouldEqual Completed
      assert(!api.hasSession)
    }
  }

  private def requireAuthorizedAccess(client: AkkaHttpClient): Unit = {
    requireAccessToUnprotected(client)
    client.get_[String](s"$localUri/authorizedUser") await 99.s shouldEqual "A-USER"
  }

  private def requireAccessIsUnauthorizedOrPublic(client: AkkaHttpClient): Unit = {
    requireAccessToUnprotected(client)
    if (isPublic) {
      requireAccessIsPublic(client)
    } else {
      requireAccessIsUnauthorized(client)
    }
  }

  private def requireAccessIsPublic(client: AkkaHttpClient): Unit = {
    assert(isPublic)
    requireAccessToUnprotected(client)
    getViaAuthorizedUsed(client)
  }

  private def requireAccessIsUnauthorized(client: AkkaHttpClient): HttpException = {
    requireAccessToUnprotected(client)
    val exception = intercept[AkkaHttpClient.HttpException] {
      getViaAuthorizedUsed(client)
    }
    assert(exception.status == Unauthorized)
    assert(exception.header[`WWW-Authenticate`] ==
      Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
    exception
  }

  private def requireAccessIsForbidden(client: AkkaHttpClient): HttpException = {
    requireAccessToUnprotected(client)
    val exception = intercept[AkkaHttpClient.HttpException] {
      getViaAuthorizedUsed(client)
    }
    assert(exception.status == Forbidden)
    assert(exception.header[`WWW-Authenticate`].isEmpty)
    exception
  }

  private def getViaAuthorizedUsed(client: AkkaHttpClient) =
    client.get_[String](s"$localUri/authorizedUser") await 99.s

  private def requireAccessToUnprotected(client: AkkaHttpClient): Unit =
    client.get_[String](s"$localUri/unprotected") await 99.s shouldEqual "THE RESPONSE"

  private def withSessionApi(headers: List[HttpHeader] = Nil)(body: SessionApi with AkkaHttpClient => Unit): Unit = {
    val api = new SessionApi with AkkaHttpClient {
      def httpClient = this: AkkaHttpClient
      def sessionUri = s"$baseUri/session"
      val actorSystem = SessionRouteTest.this.actorSystem
      def baseUri = server.localUri
      def uriPrefixPath = ""
      override val standardHeaders = headers ::: super.standardHeaders
    }
    autoClosing(api) {
      body
    }
  }
}

final class SessionRouteIsNotPublicTest extends SessionRouteTest(isPublic = false)

final class SessionRouteIsPublicTest extends SessionRouteTest(isPublic = true)

object SessionRouteTest {
  private val logger = Logger(getClass)
  private val AUserAndPassword = UserAndPassword(UserId("A-USER"), SecretString("A-PASSWORD"))
  private val BUserAndPassword = UserAndPassword(UserId("B-USER"), SecretString("B-PASSWORD"))
}
