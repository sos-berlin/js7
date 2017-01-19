package com.sos.scheduler.engine.common.sprayutils.web.auth

import akka.actor.ActorSystem
import akka.testkit.TestDuration
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.{UserAndPassword, UserId}
import com.sos.scheduler.engine.common.sprayutils.JsObjectMarshallers.JsObjectMarshaller
import com.sos.scheduler.engine.common.sprayutils.web.auth.GateKeeperTest._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.IntelliJUtils.intelliJuseImports
import java.time.Instant.now
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext
import spray.http.HttpHeaders.Authorization
import spray.http.StatusCodes.{Forbidden, OK, Unauthorized}
import spray.http.{BasicHttpCredentials, Uri}
import spray.json.JsObject
import spray.routing.Directives._
import spray.routing._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class GateKeeperTest extends FreeSpec with ScalatestRouteTest {

  private val defaultConf = GateKeeper.Configuration(
    realm = "REALM",
    invalidAuthenticationDelay = 2.s,
    providePasswordValidator = () ⇒ {
      case UserAndPassword(UserId("USER"), SecretString("PASSWORD")) ⇒ true
      case _ ⇒ false
    },
    provideAccessTokenValidator = () ⇒ Map(SecretString("GRETA-TOKEN") → UserId("GRETA")))

  private def route(conf: GateKeeper.Configuration): Route = route(newGateKeeper(conf))

  private def route(gateKeeper: GateKeeper): Route =
    gateKeeper.restrict.apply {
      path("TEST") {
        (get | post) {
          complete(OK)
        }
      }
    }

  private val uri = Uri("/TEST")

  "httpIsPublic" - {
    val conf = defaultConf.copy(httpIsPublic = true)

    "Request via secured HTTP" - {
      "GET without authentication is rejected" in {
        Get(uri) ~> route(newGateKeeper(conf, isUnsecuredHttp = false)) ~> check {
          assert(!handled)
          assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
        }
      }

      "POST JSON without authentication is rejected" in {
        Post(uri, JsObject()) ~> route(newGateKeeper(conf, isUnsecuredHttp = false)) ~> check {
          assert(!handled)
          assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
        }
      }

      addPostTextPlainText(conf)
    }

    "Request via inSecuredHttp" - {
      "GET without authentication is accepted" in {
        Get(uri) ~> route(newGateKeeper(conf, isUnsecuredHttp = true)) ~> check {
          assert(status == OK)
        }
      }

      "POST JSON without authentication is accepted" in {
        Post(uri, JsObject()) ~> route(newGateKeeper(conf, isUnsecuredHttp = true)) ~> check {
          assert(status == OK)
        }
      }

      addPostTextPlainText(conf)
    }
  }

  "getIsPublic - HTTP GET is open for everybody" - {
    val conf = defaultConf.copy(getIsPublic = true)

    "GET" in {
      Get(uri) ~> route(conf) ~> check {
        assert(status == OK)
      }
    }

    "POST JSON" in {
      Post(uri, JsObject()) ~> route(conf) ~> check {
        assert(!handled)
        assert(rejections.head.isInstanceOf[AuthenticationFailedRejection])
      }
    }

    addPostTextPlainText(conf)
  }

  "User ID and password" - {
    "Access with valid credentials" in {
      Get(uri) ~> Authorization(BasicHttpCredentials("USER", "PASSWORD")) ~> route(defaultConf) ~> check {
        assert(status == OK)
      }
    }

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
  }

  "Access token" - {
    "Access with valid access token" in {
      Get(uri) ~> Authorization(BasicHttpCredentials("", "GRETA-TOKEN")) ~> route(defaultConf) ~> check {
        assert(status == OK)
      }
    }

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
      Get(uri) ~> Authorization(BasicHttpCredentials("", "WRONG")) ~> route(defaultConf) ~> check {
        assert(status == Unauthorized)
      }
      assert(now - t >= defaultConf.invalidAuthenticationDelay - 50.ms)  // Allow for timer rounding
    }
  }

  private def addPostTextPlainText(conf: GateKeeper.Configuration): Unit = {
    "POST text/plain is rejected due to CSRF" in {
      Post(uri, "TEXT") ~> route(newGateKeeper(conf, isUnsecuredHttp = true)) ~> check {
        assert(status == Forbidden)
      }
    }
  }
}

private object GateKeeperTest {
  intelliJuseImports(JsObjectMarshaller)

  def newGateKeeper(conf: GateKeeper.Configuration, isUnsecuredHttp: Boolean = false)(implicit ec: ExecutionContext) =
    new GateKeeper(conf, new CSRF(CSRF.Configuration.Default), isUnsecuredHttp = isUnsecuredHttp)
}
