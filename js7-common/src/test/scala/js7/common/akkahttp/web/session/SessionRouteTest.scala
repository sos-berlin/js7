package js7.common.akkahttp.web.session

import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.StatusCodes.{BadRequest, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.server.Directive
import akka.http.scaladsl.testkit.RouteTestTimeout
import js7.base.auth.{SessionToken, SimpleUser, UserAndPassword, UserId}
import js7.base.generic.{Completed, SecretString}
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.session.SessionApi
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.{HttpClient, Uri}
import js7.common.akkahttp.StandardDirectives.lazyRoute
import js7.common.akkahttp.web.session.SessionRouteTest.*
import js7.common.http.AkkaHttpClient
import js7.common.http.AkkaHttpClient.HttpException
import js7.data.problems.InvalidLoginProblem
import js7.data.session.HttpSessionApi
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*
import scala.collection.immutable.Set
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
sealed abstract class SessionRouteTest(override protected val isPublic: Boolean)
extends AnyFreeSpec with SessionRouteTester
{
  protected final implicit def scheduler = Scheduler.traced
  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(10.s)

  override protected[session] val specificLoginRequiredProblem = Problem.pure("specificLoginRequired")

  override protected[session] lazy val preAuthenticate = Directive(inner =>
    seal {
      lazyRoute(preAuthenticateResult match {
        case None => gateKeeper.preAuthenticate.tapply(inner)  // Production behaviour
        case Some(testResult) => inner(Tuple1(testResult))     // For test, to simulate HTTPS multi-user distinguished names
      })
    })

  private var preAuthenticateResult: Option[Either[Set[UserId], SimpleUser]] = None

  import gateKeeper.invalidAuthenticationDelay

  "login fails if server is unreachable" in {
    withSessionApi(None) { api =>
      // In the rare case of an already used TCP port (alien software) akka.http.scaladsl.model.IllegalResponseException may be returned, crashing the test.
      val exception = intercept[akka.stream.StreamTcpException] {
        api.login() await 99.s
      }
      assert(exception.getMessage contains "java.net.ConnectException")
    }
  }

  "loginUntilReachable" - {
    "invalid authorization" in {
      withSessionApi(Some(UserId("INVALID") -> SecretString("INVALID"))) { api =>
        import api.implicitSessionToken
        @volatile var count = 0
        def onError(t: Throwable) = Task {
          count += 1
          logger.debug(s"count=$count " + t.toStringWithCauses)
          true
        }
        val runningSince = now
        val whenLoggedIn = api.loginUntilReachable(
          Iterator.continually(10.ms),
          onError = onError
        ).runToFuture
        // Akka delays 100ms, 200ms, 400ms: "Connection attempt failed. Backing off new connection attempts for at least 100 milliseconds"
        waitForCondition(99.s, 10.ms)(count >= 3)
        assert(count >= 3)
        server.start await 99.s
        val exception = intercept[AkkaHttpClient.HttpException] {
          whenLoggedIn await 99.s
        }
        assert(exception.status == Unauthorized)
        assert(runningSince.elapsed >= invalidAuthenticationDelay)
        requireAccessIsUnauthorizedOrPublic(api)
      }
    }

    "authorized" in {
      withSessionApi(Some(AUserAndPassword)) { api =>
        import api.implicitSessionToken
        api.loginUntilReachable(Iterator.continually(10.ms), _ => Task.pure(true)) await 99.s
        requireAuthorizedAccess(api)
        api.logout() await 99.s
        requireAccessIsUnauthorizedOrPublic(api)
      }
    }
  }

  "Login without credentials is accepted as Anonymous but access is nevertheless unauthorized if not public" in {
    withSessionApi(None) { api =>
      import api.implicitSessionToken
      api.login() await 99.s
      requireAccessIsUnauthorizedOrPublic(api)
    }
  }

  "Login without credentials and with wrong Authorization header is rejected with 401 Unauthorized and delayed" in {
    withSessionApi(None, Right(Authorization(BasicHttpCredentials("A-USER", "")) :: Nil)) { api =>
      import api.implicitSessionToken
      val runningSince = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login() await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(runningSince.elapsed >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(api)
    }
  }

  "Login without credentials and with Anonymous Authorization header is rejected and delayed" in {
    withSessionApi(None, Right(Authorization(BasicHttpCredentials(UserId.Anonymous.string, "")) :: Nil)) { api =>
      import api.implicitSessionToken
      val runningSince = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login() await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
      assert(runningSince.elapsed >= invalidAuthenticationDelay)
      requireAccessIsUnauthorized(api)
    }
  }

  "Login with credentials and HTTP or HTTPS authorization" - {
    "Credentials match" in {
      withSessionApi(Some(AUserAndPassword), Right(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil)) { api =>
        import api.implicitSessionToken
        requireAuthorizedAccess(api)
        api.login() await 99.s
        assert(api.hasSession)
      }
    }

    "Login password does not match" in {
      withSessionApi(
        Some(AUserAndPassword.copy(password = SecretString("WRONG"))),
        Right(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil))
      { api =>
        import api.implicitSessionToken
        requireAuthorizedAccess(api)
        val exception = intercept[AkkaHttpClient.HttpException] {
          api.login() await 99.s
        }
        assert(exception.status == Unauthorized)
        assert(exception.problem == Some(InvalidLoginProblem))
        requireAuthorizedAccess(api)
      }
    }

    "Login with credentials and different Authorization header is rejected" in {
      withSessionApi(
        Some(UserAndPassword(UserId("B-USER"), SecretString("B-PASSWORD"))),
        Right(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil))
      { api =>
        import api.implicitSessionToken
        requireAuthorizedAccess(api)
        val exception = intercept[AkkaHttpClient.HttpException] {
          api.login() await 99.s
        }
        assert(exception.status == BadRequest &&
          exception.problem == Some(Problem("Login user does not match HTTP(S) user")))
      }
    }

    "Login with credentials and equivalent Authorization header but wrong password is rejected" in {
      withSessionApi(
        Some(UserAndPassword(UserId("A-USER"), SecretString("WRONG-PASSWORD"))),
        Right(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil))
      { api =>
        import api.implicitSessionToken
        requireAuthorizedAccess(api)
        val exception = intercept[AkkaHttpClient.HttpException] {
          api.login() await 99.s
        }
        assert(exception.status == Unauthorized && exception.problem == Some(InvalidLoginProblem))
      }
    }

    "Login with credentials and equivalent Authorization header is accepted" in {
      withSessionApi(
        Some(UserAndPassword(UserId("A-USER"), SecretString("A-PASSWORD"))),
        Right(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil))
      { api =>
        import api.implicitSessionToken
        requireAuthorizedAccess(api)
        api.login() await 99.s
        requireAuthorizedAccess(api)
      }
    }
  }

  "Login with credentials and HTTPS multi-UserId distinguished names" - {
    "Credentials match" in {
      withSessionApi(Some(AUserAndPassword), Left(Left(Set(UserId("A-USER"), UserId("B-USER"))))) { api =>
        import api.implicitSessionToken
        requireAccessIsUnauthorizedOrPublic(api)
        api.login() await 99.s
        assert(api.hasSession)
      }
    }

    "Login UserId does not match" in {
      withSessionApi(
        Some(AUserAndPassword),
        Left(Left(Set(UserId("X-USER"), UserId("B-USER")))))
      { api =>
        import api.implicitSessionToken
        requireAccessIsUnauthorizedOrPublic(api)
        val exception = intercept[AkkaHttpClient.HttpException] {
          api.login() await 99.s
        }
        assert(exception.status == BadRequest &&
          exception.problem == Some(specificLoginRequiredProblem))
      }
    }

    "Login password does not match" in {
      withSessionApi(
        Some(AUserAndPassword.copy(password = SecretString("WRONG"))),
        Left(Left(Set(UserId("A-USER"), UserId("B-USER")))))
      { api =>
        import api.implicitSessionToken
        requireAccessIsUnauthorizedOrPublic(api)
        val exception = intercept[AkkaHttpClient.HttpException] {
          api.login() await 99.s
        }
        assert(exception.status == Unauthorized)
        assert(exception.problem == Some(InvalidLoginProblem))
      }
    }

    "Login with credentials but wrong password is rejected" in {
      withSessionApi(
        Some(UserAndPassword(UserId("A-USER"), SecretString("WRONG-PASSWORD"))),
        Left(Left(Set(UserId("A-USER"), UserId("B-USER")))))
      { api =>
        import api.implicitSessionToken
        requireAccessIsUnauthorizedOrPublic(api)
        val exception = intercept[AkkaHttpClient.HttpException] {
          api.login() await 99.s
        }
        assert(exception.status == Unauthorized && exception.problem == Some(InvalidLoginProblem))
      }
    }
  }

  "Login without credentials but with Authorization header is accepted" in {
    withSessionApi(None, Right(Authorization(BasicHttpCredentials("A-USER", "A-PASSWORD")) :: Nil)) { api =>
      import api.implicitSessionToken
      requireAuthorizedAccess(api)
      api.login() await 99.s
      assert(api.hasSession)
      requireAuthorizedAccess(api)
    }
  }

  "Login as 'Anonymous' is rejected" in {
    withSessionApi(Some(UserId.Anonymous -> SecretString(""))) { api =>
      import api.implicitSessionToken
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login() await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(HttpChallenges.basic("TEST REALM") :: Nil)))

      requireAccessIsUnauthorizedOrPublic(api)  // public=true allows access
    }
  }

  "Login with invalid credentials is rejected with 403 Unauthorized and delayed" in {
    withSessionApi(Some(UserId("A-USER") -> SecretString(""))) { api =>
      import api.implicitSessionToken
      val runningSince = now
      val exception = intercept[AkkaHttpClient.HttpException] {
        api.login() await 99.s
      }
      assert(exception.status == Unauthorized)
      assert(exception.header[`WWW-Authenticate`] ==
        Some(`WWW-Authenticate`(HttpChallenges.basic("TEST REALM") :: Nil)))
      assert(exception.problem == Some(InvalidLoginProblem))
      assert(runningSince.elapsed >= invalidAuthenticationDelay)

      requireAccessIsUnauthorizedOrPublic(api)  // public=true allows access
    }
  }

  "Login and Logout" in {
    withSessionApi(Some(AUserAndPassword)) { api =>
      import api.implicitSessionToken
      assert(!api.hasSession)

      if (isPublic) {
          // Access without Login (Session) is allowed
        api.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s
      } else {
        // Access without Login (Session) is rejected
        intercept[HttpException] {
          api.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s
        }.status shouldEqual Unauthorized
        assert(!api.hasSession)
      }

      // Login and Logout
      api.login() await 99.s
      val Some(sessionToken) = api.sessionToken
      assert(api.hasSession)
      requireAuthorizedAccess(api)
      assert(api.hasSession)
      api.logout() await 99.s shouldEqual Completed
      assert(!api.hasSession)

      // Using old SessionToken is Unauthorized
      api.setSessionToken(sessionToken)
      val exception = requireAccessIsForbidden(api)
      assert(HttpClient.sessionMayBeLost(exception))
    }
  }

  "Logout without SessionToken is short-circuited" in {
    withSessionApi(None) { api =>
      assert(!api.hasSession)
      api.logout() await 99.s shouldEqual Completed
      assert(!api.hasSession)
    }
  }

  "Use of discarded SessionToken is Forbidden, clearSession" in {
    // This applies to all commands, also Login and Logout.
    // With Unauthorized or Forbidden, stateful SessionApi learns about the invalid session.
    withSessionApi(Some(AUserAndPassword)) { api =>
      api.setSessionToken(SessionToken(SecretString("DISCARDED")))
      import api.implicitSessionToken
      val exception = requireAccessIsForbidden(api)
      assert(HttpClient.sessionMayBeLost(exception))
      assert(api.hasSession)

      api.clearSession()
      api.login() await 99.s
      assert(api.hasSession)
      api.logout() await 99.s
    }
  }

  "logout clears SessionToken even if unknown" in {
    withSessionApi(None) { api =>
      api.setSessionToken(SessionToken(SecretString("DISCARDED")))
      assert(api.hasSession)
      api.logout() await 99.s
      assert(!api.hasSession)
    }
  }

  "Login with SessionToken" - {
    "Known SessionToken is invalidated" in {
      withSessionApi(None) { api =>
        api.login_(Some(AUserAndPassword)) await 99.s
        val Some(firstSessionToken) = api.sessionToken
        assert(api.hasSession)
        api.login_(Some(BUserAndPassword)) await 99.s
        assert(api.hasSession)

        withSessionApi(None) { otherClient =>
          // Using old SessionToken is Unauthorized
          otherClient.setSessionToken(firstSessionToken)
          import otherClient.implicitSessionToken
          val exception = intercept[AkkaHttpClient.HttpException] {
            otherClient.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s
          }
          requireAccessIsForbidden(otherClient)
          assert(HttpClient.sessionMayBeLost(exception))
        }

        api.logout() await 99.s shouldEqual Completed
        assert(!api.hasSession)
      }
    }

    "Unknown SessionToken is ignored" in {
      withSessionApi(Some(AUserAndPassword)) { api =>
        val unknown = SessionToken(SecretString("UNKNKOWN"))
        api.setSessionToken(unknown)
        api.login() await 99.s
        assert(api.sessionToken.exists(_ != unknown))
        api.logout() await 99.s shouldEqual Completed
      }
    }
  }

  private def withSessionApi(
    userAndPassword_ : Option[UserAndPassword],
    idsOrUserOrHeaders: Either[Either[Set[UserId], SimpleUser], List[HttpHeader]] = Right(Nil))
    (body: HttpSessionApi & AkkaHttpClient & SessionApi.HasUserAndPassword => Unit)
  : Unit = {
    val api = new HttpSessionApi with AkkaHttpClient with SessionApi.HasUserAndPassword {
      protected val name = "SessionRouteTest"
      protected def userAndPassword = userAndPassword_
      def httpClient = this: AkkaHttpClient
      def sessionUri = Uri(s"$baseUri/session")
      val actorSystem = SessionRouteTest.this.system
      def baseUri = localUri
      def uriPrefixPath = ""
      override val standardHeaders = idsOrUserOrHeaders.toOption.toList.flatten ::: super.standardHeaders
      def httpsConfig = HttpsConfig.empty
    }
    autoClosing(api) { _ =>
      val saved = preAuthenticateResult
      preAuthenticateResult = idsOrUserOrHeaders.left.toOption
      try body(api)
      finally preAuthenticateResult = saved
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
