package js7.common.pekkohttp.web.session

import cats.effect.Deferred
import cats.effect.unsafe.IORuntime
import js7.base.Js7Version
import js7.base.auth.{HashedPassword, SessionToken, SimpleUser, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.test.{OurTestSuite}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.auth.IdToUser
import js7.common.http.PekkoHttpClient.`x-js7-session`
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.RouteProviderTest.*
import org.apache.pekko.http.scaladsl.model.StatusCodes.{Forbidden, OK, Unauthorized}
import org.apache.pekko.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class RouteProviderTest extends OurTestSuite, RouteProvider, ScalatestRouteTest:

  private given IORuntime = ioRuntime

  override def testConfig = config"pekko.loglevel = warning"
    .withFallback(super.testConfig)

  protected type OurSession = SimpleSession

  protected def whenShuttingDown = Deferred.unsafe
  protected val config = config"js7.web.server.verbose-error-messages = on"

  protected lazy val sessionRegister =
    SessionRegister.forTest(SimpleSession.apply, SessionRegister.TestConfig)

  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(99.s)

  protected lazy val gateKeeper = new GateKeeper(
    WebServerBinding.Http,
    GateKeeper.Configuration[SimpleUser](
      realm = "TEST-REALM",
      invalidAuthenticationDelay = 100.millis,
      idToUser = IdToUser.fromConfig(
        config"""js7.auth.users.TEST-USER: "plain:123" """,
        SimpleUser.apply),
      distinguishedNameToIdsOrUser = Map.empty))

  private var sessionToken = SessionToken(SecretString("INVALID"))

  private val route = Route.seal(
    path("authorizedUser") {
      authorizedUser() { user =>
        complete("authorizedUser=" + user.id.string)
      }
    } ~
    path("sessionOption") {
      gateKeeper.preAuthenticate { idsOrUser =>
        sessionOption(idsOrUser) {
          case None => complete("NO SESSION")
          case Some(session) => complete("userId=" + session.currentUser.id.string)
        }
      }
    })

  "authenticatedUser" - {
    "Anonymous" in:
      Get("/authorizedUser") ~> route ~> check:
        assert(status == OK)
        assert(responseAs[String] == "authorizedUser=Anonymous")

    "TEST-USER, wrong password" in:
      Get("/authorizedUser") ~> Authorization(BasicHttpCredentials("TEST-USER", "xxx")) ~> route ~> check:
        assert(status == Unauthorized)
    "TEST-USER, right password" in:
      Get("/authorizedUser") ~> Authorization(BasicHttpCredentials("TEST-USER", "123")) ~> route ~> check:
        assert(status == OK)
        assert(responseAs[String] == "authorizedUser=TEST-USER")
  }

  "sessionOption" - {
    "No session header" in:
      Get("/sessionOption") ~> route ~> check:
        assert(status == OK)
        assert(responseAs[String] == "NO SESSION")

    "Unknown session header is rejected" in:
      Get("/sessionOption") ~> addHeader(`x-js7-session`.name, "UNKNOWN") ~> route ~> check:
        assert(status == Forbidden)

    "Known SessionToken" in:
      sessionToken = sessionRegister.login(TestUser, Some(Js7Version)).await(99.s)
      Get("/sessionOption") ~> addHeader(`x-js7-session`.name, sessionToken.secret.string) ~> route ~> check:
        assert(status == OK)
        assert(responseAs[String] == "userId=TEST-USER")
      Get("/authorizedUser") ~> addHeader(`x-js7-session`.name, sessionToken.secret.string) ~> route ~> check:
        assert(status == OK)
        assert(responseAs[String] == "authorizedUser=TEST-USER")
  }

object RouteProviderTest:
  private val TestUser = SimpleUser(UserId("TEST-USER"), HashedPassword(SecretString("321"), _.reverse))

  final case class MySession(sessionInit: SessionInit) extends Session
