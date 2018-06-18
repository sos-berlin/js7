package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.{OK, Unauthorized}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.base.auth.{HashedPassword, SessionToken, SimpleUser, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.RouteProviderTest._
import com.sos.jobscheduler.common.auth.IdToUser
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.typesafe.config.ConfigFactory
import java.time.Instant.now
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RouteProviderTest extends FreeSpec with RouteProvider with ScalatestRouteTest {

  protected type Session = MySession

  implicit protected def scheduler = Scheduler.global
  protected lazy val sessionRegister = SessionRegister.start[MySession](system, MySession.apply,
    sessionTimeout = 1.hour, akkaAskTimeout = 99.seconds)
  private implicit val timeout = RouteTestTimeout(10.seconds)

  protected val gateKeeper = new GateKeeper(
    GateKeeper.Configuration[SimpleUser](
      realm = "TEST-REALM",
      invalidAuthenticationDelay = 100.ms,
      idToUser = IdToUser.fromConfig(
        ConfigFactory.parseString("""jobscheduler.auth.users.TEST-USER: "plain:123" """),
        SimpleUser.apply)),
    new TimerService(Some(1.s)))

  private var sessionToken = SessionToken(SecretString("INVALID"))

  private val route = Route.seal(
    path("authorizedUser") {
      authorizedUser() { user ⇒
        complete("authorizedUser=" + user.id.string)
      }
    } ~
    path("sessionOption") {
      sessionOption() {
        case None ⇒ complete("NO SESSION")
        case Some(session) ⇒ complete("userId=" + session.user.id.string)
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
      val t = now
      Get("/sessionOption") ~> addHeader(SessionToken.HeaderName, "UNKNOWN") ~> route ~> check {
        assert(status == Unauthorized)
      }
      assert(now - t >= gateKeeper.invalidAuthenticationDelay)
    }

    "Known SessionToken" in {
      sessionToken = sessionRegister.login(TestUser).await(99.seconds)(scheduler)
      Get("/sessionOption") ~> addHeader(SessionToken.HeaderName, sessionToken.secret.string) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[String] == "userId=TEST-USER")
      }
      Get("/authorizedUser") ~> addHeader(SessionToken.HeaderName, sessionToken.secret.string) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[String] == "authorizedUser=TEST-USER")
      }
    }
  }
}

object RouteProviderTest {
  private val TestUser = SimpleUser(UserId("TEST-USER"), HashedPassword(SecretString("321"), _.reverse))

  final case class MySession(sessionInit: SessionInit[SimpleUser]) extends LoginSession {
    type User = SimpleUser
  }
}
