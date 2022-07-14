package js7.common.akkahttp.web.session

import akka.http.scaladsl.model.StatusCodes.{Forbidden, ServiceUnavailable, Unauthorized}
import akka.http.scaladsl.model.headers.{HttpChallenges, `WWW-Authenticate`}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.base.auth.{SessionToken, UserId, ValidUserPermission}
import js7.base.configutils.Configs.*
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkautils.Akkas
import js7.common.http.AkkaHttpClient
import js7.common.http.AkkaHttpClient.HttpException
import monix.eval.Task
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.Future

trait SessionRouteTester extends BeforeAndAfterAll with ScalatestRouteTest with SessionRoute
{
  this: Suite =>

  coupleScribeWithSlf4j()

  protected def isPublic = false

  protected type Session = SimpleSession

  protected final def whenShuttingDown = Future.never
  protected final val config = config"js7.web.server.verbose-error-messages = on"

  override def testConfig = config"""
    js7.web.client.compression = off
    akka.http.host-connection-pool.max-connection-backoff = 10ms
  """.withFallback(super.testConfig)

  private implicit def implicitScheduler = scheduler

  protected final lazy val gateKeeper = GateKeeper.forTest(
    isPublic = isPublic,
    config = config"""
      js7.auth.users {
        A-USER = "plain:A-PASSWORD"
        B-USER = "plain:B-PASSWORD"
      }""")

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

  protected final lazy val server = AkkaWebServer.forTest()(route)

  protected lazy val localUri = server.localUri

  override def afterAll() = {
    Akkas.shutDownHttpConnectionPools(system)
    server.stop().await(99.s)
    super.afterAll()
  }

  protected final def requireAuthorizedAccess(client: AkkaHttpClient, expectedUserId: UserId = UserId("A-USER"))
    (implicit s: Task[Option[SessionToken]]): Unit = {
    requireAccessToUnprotected(client)
    client.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s shouldEqual expectedUserId.string
  }

  protected final def requireAccessIsUnauthorizedOrPublic(client: AkkaHttpClient)(implicit s: Task[Option[SessionToken]]): Unit = {
    requireAccessToUnprotected(client)
    if (isPublic) {
      requireAccessIsPublic(client)
    } else {
      requireAccessIsUnauthorized(client)
    }
  }

  protected final def requireAccessIsPublic(client: AkkaHttpClient)(implicit s: Task[Option[SessionToken]]): Unit = {
    assert(isPublic)
    requireAccessToUnprotected(client)
    getViaAuthorizedUsed(client)
  }

  protected final def requireAccessIsUnauthorized(client: AkkaHttpClient)(implicit s: Task[Option[SessionToken]]): HttpException = {
    requireAccessToUnprotected(client)
    val exception = intercept[AkkaHttpClient.HttpException] {
      getViaAuthorizedUsed(client)
    }
    assert(exception.status == Unauthorized)
    assert(exception.header[`WWW-Authenticate`] ==
      Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
    exception
  }

  protected final def requireAccessIsForbidden(client: AkkaHttpClient)(implicit s: Task[Option[SessionToken]]): HttpException = {
    requireAccessToUnprotected(client)
    val exception = intercept[AkkaHttpClient.HttpException] {
      getViaAuthorizedUsed(client)
    }
    assert(exception.status == Forbidden)
    assert(exception.header[`WWW-Authenticate`].isEmpty)
    exception
  }

  private def getViaAuthorizedUsed(client: AkkaHttpClient)(implicit s: Task[Option[SessionToken]]) =
    client.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s

  protected final def requireAccessToUnprotected(client: AkkaHttpClient)(implicit s: Task[Option[SessionToken]]): Unit =
    client.get_[String](Uri(s"$localUri/unprotected")) await 99.s shouldEqual "THE RESPONSE"
}
