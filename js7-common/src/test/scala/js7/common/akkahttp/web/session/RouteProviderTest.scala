package js7.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.{Forbidden, OK, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import js7.base.Js7Version
import js7.base.auth.{HashedPassword, SessionToken, SimpleUser, UserId}
import js7.base.configutils.Configs._
import js7.base.generic.SecretString
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.RouteProviderTest._
import js7.common.auth.IdToUser
import js7.common.http.AkkaHttpClient.`x-js7-session`
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RouteProviderTest extends AnyFreeSpec with RouteProvider with ScalatestRouteTest
{
  coupleScribeWithSlf4j()

  protected type Session = MySession

  protected def whenShuttingDown = Future.never
  implicit protected def scheduler = Scheduler.global
  protected val config = config"js7.web.server.verbose-error-messages = on"
  protected lazy val sessionRegister = SessionRegister.start[MySession](system, MySession.apply, SessionRegister.TestConfig)
  private implicit val routeTestTimeout = RouteTestTimeout(99.s)

  protected val gateKeeper = new GateKeeper(
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
    "Anonymous" in {
      Get("/authorizedUser") ~> route ~> check {
        assert(status == OK)
        assert(responseAs[String] == "authorizedUser=Anonymous")
      }
    }

    "TEST-USER, wrong password" in {
      Get("/authorizedUser") ~> Authorization(BasicHttpCredentials("TEST-USER", "xxx")) ~> route ~> check {
        assert(status == Unauthorized)
      }
    }
    "TEST-USER, right password" in {
      Get("/authorizedUser") ~> Authorization(BasicHttpCredentials("TEST-USER", "123")) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[String] == "authorizedUser=TEST-USER")
      }
    }
  }

  "sessionOption" - {
    "No session header" in {
      Get("/sessionOption") ~> route ~> check {
        assert(status == OK)
        assert(responseAs[String] == "NO SESSION")
      }
    }

    "Unknown session header is rejected" in {
      Get("/sessionOption") ~> addHeader(`x-js7-session`.name, "UNKNOWN") ~> route ~> check {
        assert(status == Forbidden)
      }
    }

    "Known SessionToken" in {
      sessionToken = sessionRegister.login(TestUser, Some(Js7Version)).await(99.s).orThrow
      Get("/sessionOption") ~> addHeader(`x-js7-session`.name, sessionToken.secret.string) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[String] == "userId=TEST-USER")
      }
      Get("/authorizedUser") ~> addHeader(`x-js7-session`.name, sessionToken.secret.string) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[String] == "authorizedUser=TEST-USER")
      }
    }
  }
}

object RouteProviderTest {
  private val TestUser = SimpleUser(UserId("TEST-USER"), HashedPassword(SecretString("321"), _.reverse))

  final case class MySession(sessionInit: SessionInit[SimpleUser]) extends Session {
    type User = SimpleUser
  }
}
