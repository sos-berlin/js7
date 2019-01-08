package com.sos.jobscheduler.common.akkahttp.web.session

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.StatusCodes.{BadRequest, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, HttpChallenge, HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.base.auth.{SessionToken, SimpleUser, UserAndPassword, UserId, ValidUserPermission}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.web.session.RouteProvider.LoginWWWAuthenticate
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRouteTest._
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
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
final class SessionRouteTest extends FreeSpec with BeforeAndAfterAll with ScalatestRouteTest with SessionRoute {

  protected type Session = SimpleSession

  implicit protected def scheduler = Scheduler.global
  protected val config = ConfigFactory.parseString("jobscheduler.webserver.verbose-error-messages = on")
  private lazy val actorSystem = ActorSystem("SessionRouteTest")

  protected lazy val gateKeeper = new GateKeeper(
    GateKeeper.Configuration.fromConfig(TestConfig, SimpleUser.apply))

  protected lazy val sessionRegister =
    SessionRegister.start[SimpleSession](system, SimpleSession.apply, SessionRegister.TestConfig)

  private implicit val routeTestTimeout = RouteTestTimeout(10.seconds)

  private val route = Route.seal(
    decodeRequest {
      pathSegment("session") {
        sessionRoute
      } ~
      path("test") {
        get {
          authorizedUser(ValidUserPermission) { user ⇒
            complete(user.id.string)
          }
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

  override def beforeAll() = {
    super.beforeAll()
    server.start() await 99.s
  }

  override def afterAll() = {
    server.close()
    actorSystem.terminate() await 99.s
    super.afterAll()
  }

  "Login without credentials is rejected with 401 Unauthorized" in {
    withClient() { client ⇒
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.login(None) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(now - t >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(client)
    }
  }

  "Login without credentials and with wrong Authorization header is rejected with 401 Unauthorized" in {
    withClient(Authorization(BasicHttpCredentials("A-USER", "")) :: Nil) { client ⇒
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.login(None) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(now - t >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(client)
    }
  }

  "Login without credentials and with Anonymous Authorization header is rejected" in {
    withClient(Authorization(BasicHttpCredentials(UserId.Anonymous.string, "")) :: Nil) { client ⇒
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.login(None) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(now - t >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(client)
    }
  }

  "Login with credentials and with Authorization header is rejected" in {
    withClient(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil) { client ⇒
      requireAccess(client)
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.login(Some(AUserAndPassword)) await 99.s
      }
      assert(exception.status == BadRequest)
      assert(exception.dataAsString.parseJsonOrThrow.as[Problem] == Right(Problem("Both command Login and HTTP header Authentication?")))
      requireAccess(client)
    }
  }

  "Login without credentials but with Authorization header is accepted" in {
    withClient(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil) { client ⇒
      client.login(None) await 99.s
      assert(client.hasSession)
      requireAccess(client)
    }
  }

  "Login as Anonymous is rejected" in {
    withClient() { client ⇒
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.login(Some(UserId.Anonymous → SecretString(""))) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenge("X-JobScheduler-Login", realm = None)))))
      assert(now - t >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(client)
    }
  }

  "Login with invalid credentials is rejected with 403 Unauthorized" in {
    withClient() { client ⇒
      val t = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        client.login(Some(UserId("A-USER") → SecretString(""))) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenge("X-JobScheduler-Login", realm = None)))))
      assert(exception.dataAsString contains "Login: unknown user or invalid password")
      assert(now - t >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(client)
    }
  }

  "Login and Logout" in {
    withClient() { client ⇒
      assert(!client.hasSession)

      // Access without Login (Session) is rejected
      intercept[HttpException] {
        client.get_[String](s"$localUri/test") await 99.s
      }.status shouldEqual Unauthorized
      assert(!client.hasSession)

      // Login and Logout
      val sessionToken = client.login(Some(AUserAndPassword)) await 99.s
      assert(client.hasSession)
      requireAccess(client)
      assert(client.hasSession)
      client.logout() await 99.s shouldEqual Completed
      assert(!client.hasSession)

      // Using old SessionToken is Unauthorized
      client.setSessionToken(sessionToken)
      val exception = requireAccessIsUnauthorized(client)
      assert(exception.header[`WWW-Authenticate`] == Some(LoginWWWAuthenticate))
      assert(AkkaHttpClient.sessionMayBeLost(exception))
    }
  }

  "Logout without SessionToken is short-circuited" in {
    withClient() { client ⇒
      assert(!client.hasSession)
      client.logout() await 99.s shouldEqual Completed
      assert(!client.hasSession)
    }
  }

  "Use of discarded SessionToken is unauthorized, clearSession" in {
    // This applies to all commands, also Login and Logout.
    // With Unauthorized or Forbidden, the client learns about the invalid session.
    withClient() { client ⇒
      client.setSessionToken(SessionToken(SecretString("DISCARDED")))
      val exception = requireAccessIsUnauthorized(client)
      assert(exception.header[`WWW-Authenticate`] == Some(LoginWWWAuthenticate))
      assert(AkkaHttpClient.sessionMayBeLost(exception))
      assert(client.hasSession)

      client.clearSession()
      client.login(Some(AUserAndPassword)) await 99.s
      assert(client.hasSession)
      client.logout() await 99.s
    }
  }

  "logout clears SessionToken even if invalid" in {
    withClient() { client ⇒
      client.setSessionToken(SessionToken(SecretString("DISCARDED")))
      assert(client.hasSession)
      client.logout() await 99.s
      assert(!client.hasSession)
    }
  }

  "Second Login invalidates first Login" in {
    withClient() { client ⇒
      val firstSessionToken = client.login(Some(AUserAndPassword)) await 99.s
      assert(client.hasSession)
      client.login(Some(BUserAndPassword)) await 99.s
      assert(client.hasSession)

      withClient() { otherClient ⇒
        // Using old SessionToken is Unauthorized
        otherClient.setSessionToken(firstSessionToken)
        val exception = intercept[AkkaHttpClient.HttpException] {
          otherClient.get_[String](s"$localUri/test") await 99.s
        }
        assert(exception.status == Unauthorized)
        assert(exception.header[`WWW-Authenticate`] == Some(LoginWWWAuthenticate))
        assert(AkkaHttpClient.sessionMayBeLost(exception))
      }

      client.logout() await 99.s shouldEqual Completed
      assert(!client.hasSession)
    }
  }

  private def requireAccess(client: SessionApi with AkkaHttpClient): Unit =
    client.get_[String](s"$localUri/test") await 99.s shouldEqual "A-USER"

  private def requireAccessIsUnauthorized(client: SessionApi with AkkaHttpClient): HttpException = {
    val exception = intercept[AkkaHttpClient.HttpException] {
      client.get_[String](s"$localUri/test") await 99.s
    }
    assert(exception.status == Unauthorized)
    exception
  }

  private def withClient(headers: List[HttpHeader] = Nil)(body: SessionApi with AkkaHttpClient ⇒ Unit): Unit = {
    val client = new SessionApi with AkkaHttpClient {
      def httpClient = this: AkkaHttpClient
      def sessionUri = s"$baseUri/session"
      def actorSystem = SessionRouteTest.this.actorSystem
      def baseUri = server.localUri
      def uriPrefixPath = ""
      override val standardHeaders = headers ::: super.standardHeaders
    }
    autoClosing(client) {
      body
    }
  }
}

object SessionRouteTest {
  private val AUserAndPassword = UserAndPassword(UserId("A-USER"), SecretString("A-PASSWORD"))
  private val BUserAndPassword = UserAndPassword(UserId("B-USER"), SecretString("B-PASSWORD"))

  private val TestConfig = ConfigFactory.parseString(
    """jobscheduler.webserver.auth {
      |  realm = "TEST REALM"
      |  invalid-authentication-delay = 100ms
      |  loopback-is-public = false
      |  get-is-public = false
      |  public = false
      |}
      |
      |jobscheduler.auth.users {
      |  A-USER = "plain:A-PASSWORD"
      |  B-USER = "plain:B-PASSWORD"
      |}
      |""".stripMargin)
}
