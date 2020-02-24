package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.StatusCodes.{BadRequest, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.testkit.RouteTestTimeout
import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword, UserId}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.session.HttpSessionApi
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRouteTest._
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
sealed abstract class SessionRouteTest(override protected val isPublic: Boolean)
extends FreeSpec with SessionRouteTester
{
  protected final implicit def scheduler = Scheduler.global
  private implicit val routeTestTimeout = RouteTestTimeout(10.seconds)

  import gateKeeper.invalidAuthenticationDelay

  "login fails if server is unreachable" in {
    withSessionApi() { api =>
      // In the rare case of an already used TCP port (alien software) akka.http.scaladsl.model.IllegalResponseException may be returned, crashing the test.
      val exception = intercept[akka.stream.StreamTcpException] {
        api.login(None) await 99.s
      }
      assert(exception.getMessage contains "java.net.ConnectException")
    }
  }

  "loginUntilReachable" - {
    "invalid authorization" in {
      withSessionApi() { api =>
        @volatile var count = 0
        def onError(t: Throwable) = Task {
          count += 1
          logger.debug(s"count=$count " + t.toStringWithCauses)
        }
        val runningSince = now
        val whenLoggedIn = api.loginUntilReachable(
          Some(UserId("INVALID") -> SecretString("INVALID")),
          Iterator.continually(10.milliseconds),
          onError
        ).runToFuture
        // Akka delays 100ms, 200ms, 400ms: "Connection attempt failed. Backing off new connection attempts for at least 100 milliseconds"
        waitForCondition(99.seconds, 10.milliseconds)(count >= 3)
        assert(count >= 3)
        server.start() await 99.s
        val exception = intercept[AkkaHttpClient.HttpException] {
          whenLoggedIn await 99.s
        }
        assert(exception.status == Unauthorized)
        assert(runningSince.elapsed >= invalidAuthenticationDelay)
        requireAccessIsUnauthorizedOrPublic(api)
      }
    }

    "authorized" in {
      withSessionApi() { api =>
        api.loginUntilReachable(Some(AUserAndPassword), Iterator.continually(10.milliseconds), _ => Task.unit) await 99.s
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
      val runningSince = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login(None) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(runningSince.elapsed >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(api)
    }
  }

  "Login without credentials and with Anonymous Authorization header is rejected and delayed" in {
    withSessionApi(Authorization(BasicHttpCredentials(UserId.Anonymous.string, "")) :: Nil) { api =>
      val runningSince = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login(None) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(runningSince.elapsed >= invalidAuthenticationDelay)
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
      val runningSince = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login(Some(UserId("A-USER") -> SecretString(""))) await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(HttpChallenges.basic("TEST REALM") :: Nil)))
      assert(exception.dataAsString contains "Login: unknown user or invalid password")
      assert(runningSince.elapsed >= invalidAuthenticationDelay)

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

  "logout clears SessionToken even if unknown" in {
    withSessionApi() { api =>
      api.setSessionToken(SessionToken(SecretString("DISCARDED")))
      assert(api.hasSession)
      api.logout() await 99.s
      assert(!api.hasSession)
    }
  }

  "Login with SessionToken" - {
    "Known SessionToken is invalidated" in {
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

    "Unknown SessionToken is ignored" in {
      withSessionApi() { api =>
        val unknown = SessionToken(SecretString("UNKNKOWN"))
        api.setSessionToken(unknown)
        api.login(Some(AUserAndPassword)) await 99.s
        assert(api.sessionToken.exists(_ != unknown))
        api.logout() await 99.s shouldEqual Completed
      }
    }
  }

  private def withSessionApi(headers: List[HttpHeader] = Nil)(body: HttpSessionApi with AkkaHttpClient => Unit): Unit = {
    val api = new HttpSessionApi with AkkaHttpClient {
      protected val name = "SessionRouteTest"
      def httpClient = this: AkkaHttpClient
      def sessionUri = s"$baseUri/session"
      val actorSystem = SessionRouteTest.this.system
      def baseUri = localUri
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
