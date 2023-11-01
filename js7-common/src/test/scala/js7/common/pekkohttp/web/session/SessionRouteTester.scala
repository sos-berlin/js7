package js7.common.pekkohttp.web.session

import js7.base.auth.{SessionToken, UserId, ValidUserPermission}
import js7.base.configutils.Configs.*
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.http.PekkoHttpClient
import js7.common.http.PekkoHttpClient.HttpException
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkoutils.Pekkos
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.http.scaladsl.model.StatusCodes.{Forbidden, ServiceUnavailable, Unauthorized}
import org.apache.pekko.http.scaladsl.model.headers.{HttpChallenges, `WWW-Authenticate`}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.Future

trait SessionRouteTester extends BeforeAndAfterAll with ScalatestRouteTest with SessionRoute
{
  this: Suite =>

  coupleScribeWithSlf4j()

  protected def isPublic = false

  protected type OurSession = SimpleSession

  protected final def whenShuttingDown = Future.never
  protected final val config = config"js7.web.server.verbose-error-messages = on"

  override def testConfig = config"""
    js7.web.client.compression = off
    pekko.loglevel = warning
    pekko.http.host-connection-pool.max-connection-backoff = 10ms
  """.withFallback(super.testConfig)

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final lazy val gateKeeper = GateKeeper.forTest(
    isPublic = isPublic,
    config = config"""
      js7.auth.users {
        A-USER = "plain:A-PASSWORD"
        B-USER = "plain:B-PASSWORD"
      }""")

  protected final lazy val sessionRegister =
    SessionRegister.start(system, SimpleSession.apply, SessionRegister.TestConfig)

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

  protected final lazy val server = PekkoWebServer.forTest()(route)

  protected lazy val localUri = server.localUri

  override def afterAll() = {
    Pekkos.shutDownHttpConnectionPools(system)
    server.stop().await(99.s)
    super.afterAll()
  }

  protected final def requireAuthorizedAccess(client: PekkoHttpClient, expectedUserId: UserId = UserId("A-USER"))
    (implicit s: Task[Option[SessionToken]]): Unit = {
    requireAccessToUnprotected(client)
    client.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s shouldEqual expectedUserId.string
  }

  protected final def requireAccessIsUnauthorizedOrPublic(client: PekkoHttpClient)(implicit s: Task[Option[SessionToken]]): Unit = {
    requireAccessToUnprotected(client)
    if (isPublic) {
      requireAccessIsPublic(client)
    } else {
      requireAccessIsUnauthorized(client)
    }
  }

  protected final def requireAccessIsPublic(client: PekkoHttpClient)(implicit s: Task[Option[SessionToken]]): Unit = {
    assert(isPublic)
    requireAccessToUnprotected(client)
    getViaAuthorizedUsed(client)
  }

  protected final def requireAccessIsUnauthorized(client: PekkoHttpClient)(implicit s: Task[Option[SessionToken]]): HttpException = {
    requireAccessToUnprotected(client)
    val exception = intercept[PekkoHttpClient.HttpException] {
      getViaAuthorizedUsed(client)
    }
    assert(exception.status == Unauthorized)
    assert(exception.header[`WWW-Authenticate`] ==
      Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
    exception
  }

  protected final def requireAccessIsForbidden(client: PekkoHttpClient)(implicit s: Task[Option[SessionToken]]): HttpException = {
    requireAccessToUnprotected(client)
    val exception = intercept[PekkoHttpClient.HttpException] {
      getViaAuthorizedUsed(client)
    }
    assert(exception.status == Forbidden)
    assert(exception.header[`WWW-Authenticate`].isEmpty)
    exception
  }

  private def getViaAuthorizedUsed(client: PekkoHttpClient)(implicit s: Task[Option[SessionToken]]) =
    client.get_[String](Uri(s"$localUri/authorizedUser")) await 99.s

  protected final def requireAccessToUnprotected(client: PekkoHttpClient)(implicit s: Task[Option[SessionToken]]): Unit =
    client.get_[String](Uri(s"$localUri/unprotected")) await 99.s shouldEqual "THE RESPONSE"
}
