package com.sos.scheduler.engine.common.sprayutils.web.auth

import akka.actor.ActorSystem
import akka.testkit.TestDuration
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.UserAndPassword
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Authorization
import spray.http.StatusCodes.{OK, Unauthorized}
import spray.http.{BasicHttpCredentials, Uri}
import spray.routing.Directives._
import spray.routing._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class GateKeeperTest extends FreeSpec with ScalatestRouteTest {

  private val defaultConf = GateKeeper.Configuration(
    realm = "REALM",
    isValidUserAndPassword = {
      case UserAndPassword("USER", SecretString("PASSWORD")) ⇒ true
      case _ ⇒ false
    },
    invalidAuthenticationDelay = 2.s)

  private def route(conf: GateKeeper.Configuration): Route = route(new GateKeeper(conf))

  private def route(gateKeeper: GateKeeper): Route =
    gateKeeper.allows {
      path("TEST") {
        (get | post) {
          complete(OK)
        }
      }
    }

  private val uri = Uri("/TEST")

  "No access with missing credentials" in {
    Get(uri) ~> route(defaultConf) ~> check {
      assert(!handled)
      assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
    }
  }

  "No access with invalid credentials" in {
    implicit def default(implicit system: ActorSystem) =
      RouteTestTimeout((2 * defaultConf.invalidAuthenticationDelay).toFiniteDuration.dilated)

    val t = now
    Get(uri) ~> Authorization(BasicHttpCredentials("USER", "WRONG")) ~> route(defaultConf) ~> check {
      assert(status == Unauthorized)
    }
    assert(now - t >= defaultConf.invalidAuthenticationDelay - 50.ms)  // Allow for timer rounding
  }

  "Access with valid credentials" in {
    Get(uri) ~> Authorization(BasicHttpCredentials("USER", "PASSWORD")) ~> route(defaultConf) ~> check {
      assert(status == OK)
    }
  }

  "httpIsPublic" in {
    val conf = defaultConf.copy(httpIsPublic = true)
    Get(uri) ~> route(new GateKeeper(conf, isUnsecuredHttp = false)) ~> check {
      assert(!handled)
      assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
    }
    Post(uri) ~> route(new GateKeeper(conf, isUnsecuredHttp = false)) ~> check {
      assert(!handled)
      assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
    }
    Get(uri) ~> route(new GateKeeper(conf, isUnsecuredHttp = true)) ~> check {
      assert(status == OK)
    }
    Post(uri) ~> route(new GateKeeper(conf, isUnsecuredHttp = true)) ~> check {
      assert(status == OK)
    }
  }

  "getIsPublic - HTTP GET is open for everybody" in {
    val conf = defaultConf.copy(getIsPublic = true)
    Get(uri) ~> route(conf) ~> check {
      assert(status == OK)
    }
    Post(uri) ~> route(conf) ~> check {
      assert(!handled)
      assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
    }
  }
}
