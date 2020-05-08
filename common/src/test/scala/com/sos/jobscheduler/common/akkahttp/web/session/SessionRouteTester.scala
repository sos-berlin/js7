package com.sos.jobscheduler.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.{Forbidden, ServiceUnavailable, Unauthorized}
import akka.http.scaladsl.model.headers.{HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.server.Directives.{complete, decodeRequest, get, path, _}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.base.auth.{SessionToken, ValidUserPermission}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.log.ScribeUtils
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.typesafe.config.ConfigFactory
import org.scalatest.matchers.should.Matchers._
import org.scalatest.{BeforeAndAfterAll, Suite}

trait SessionRouteTester extends BeforeAndAfterAll with ScalatestRouteTest with SessionRoute
{
  this: Suite =>

  ScribeUtils.coupleScribeWithSlf4j()

  protected def isPublic = false

  protected type Session = SimpleSession

  protected final def isShuttingDown = false
  protected final val config = ConfigFactory.parseString("jobscheduler.webserver.verbose-error-messages = on")

  override def testConfig = ConfigFactory.parseString(s"""
     |akka.http.host-connection-pool.base-connection-backoff = 10ms
     |akka.http.host-connection-pool.max-connection-backoff = 10ms
     |""".stripMargin
  ).withFallback(super.testConfig)

  private implicit def implicitScheduler = scheduler

  protected final lazy val gateKeeper = GateKeeper.forTest(
    isPublic = isPublic,
    config = ConfigFactory.parseString("""
       |jobscheduler.auth.users {
       |  A-USER = "plain:A-PASSWORD"
       |  B-USER = "plain:B-PASSWORD"
       |}""".stripMargin))

  protected final lazy val sessionRegister =
    SessionRegister.start[SimpleSession](system, SimpleSession.apply, SessionRegister.TestConfig)

  protected def route =
    Route.seal(
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
        } ~
        path("ServiceUnavailable") {
          get {
            complete(ServiceUnavailable)
          }
        }
      })

  protected final lazy val server = new AkkaWebServer.ForTest(system, route)

  protected def localUri = server.localUri

  override def afterAll() = {
    server.close()
    system.terminate() await 99.s
    super.afterAll()
  }

  protected final def requireAuthorizedAccess(client: AkkaHttpClient)(implicit s: Option[SessionToken]): Unit = {
    requireAccessToUnprotected(client)
    client.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s shouldEqual "A-USER"
  }

  protected final def requireAccessIsUnauthorizedOrPublic(client: AkkaHttpClient)(implicit s: Option[SessionToken]): Unit = {
    requireAccessToUnprotected(client)
    if (isPublic) {
      requireAccessIsPublic(client)
    } else {
      requireAccessIsUnauthorized(client)
    }
  }

  protected final def requireAccessIsPublic(client: AkkaHttpClient)(implicit s: Option[SessionToken]): Unit = {
    assert(isPublic)
    requireAccessToUnprotected(client)
    getViaAuthorizedUsed(client)
  }

  protected final def requireAccessIsUnauthorized(client: AkkaHttpClient)(implicit s: Option[SessionToken]): HttpException = {
    requireAccessToUnprotected(client)
    val exception = intercept[AkkaHttpClient.HttpException] {
      getViaAuthorizedUsed(client)
    }
    assert(exception.status == Unauthorized)
    assert(exception.header[`WWW-Authenticate`] ==
      Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
    exception
  }

  protected final def requireAccessIsForbidden(client: AkkaHttpClient)(implicit s: Option[SessionToken]): HttpException = {
    requireAccessToUnprotected(client)
    val exception = intercept[AkkaHttpClient.HttpException] {
      getViaAuthorizedUsed(client)
    }
    assert(exception.status == Forbidden)
    assert(exception.header[`WWW-Authenticate`].isEmpty)
    exception
  }

  private def getViaAuthorizedUsed(client: AkkaHttpClient)(implicit s: Option[SessionToken]) =
    client.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s

  protected final def requireAccessToUnprotected(client: AkkaHttpClient)(implicit s: Option[SessionToken]): Unit =
    client.get_[String](Uri(s"$localUri/unprotected")) await 99.s shouldEqual "THE RESPONSE"
}
