package js7.common.pekkohttp.web.session

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO}
import io.circe.Encoder
import js7.base.auth.{SessionToken, UserId, ValidUserPermission}
import js7.base.configutils.Configs.*
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.{Allocated, Lazy}
import js7.base.web.Uri
import js7.common.http.PekkoHttpClient
import js7.common.http.PekkoHttpClient.HttpException
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkoutils.Pekkos
import org.apache.pekko.http.scaladsl.model.StatusCodes.{Forbidden, ServiceUnavailable, Unauthorized}
import org.apache.pekko.http.scaladsl.model.headers.{HttpChallenges, `WWW-Authenticate`}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.{BeforeAndAfterAll, Suite}

trait SessionRouteTester extends BeforeAndAfterAll, ScalatestRouteTest, SessionRoute:
  this: Suite =>

  private given IORuntime = ioRuntime

  protected def isPublic = false

  protected type OurSession = SimpleSession
  protected val sessionEncoder = summon[Encoder.AsObject[SimpleSession]]

  protected final val whenShuttingDown = Deferred.unsafe
  protected final val config = config"js7.web.server.verbose-error-messages = on"

  override def testConfig = config"""
    js7.web.client.compression = off
    pekko.loglevel = warning
    pekko.http.host-connection-pool.max-connection-backoff = 10ms
  """.withFallback(super.testConfig)

  protected final lazy val gateKeeper = GateKeeper.forTest(
    WebServerBinding.localhostHttp(port = 1),
    isPublic = isPublic,
    config = config"""
      js7.auth.users {
        A-USER = "plain:A-PASSWORD"
        B-USER = "plain:B-PASSWORD"
        LIST-SESSIONS {
          password = "plain:LIST-SESSIONS-PASSWORD"
          permissions = [ ReadInternals ]
        }
      }""")

  private val sessionRegisterLazy = Lazy[Allocated[IO, SessionRegister[SimpleSession]]]:
    SessionRegister.service(SimpleSession.apply, SessionRegister.TestConfig)
      .toAllocated.await(99.s)
  protected final lazy val sessionRegister = sessionRegisterLazy.value.allocatedThing

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

  private lazy val (localUri_, webServerResource) = PekkoWebServer
    .testUriAndResource()(route)

  protected lazy val localUri = localUri_
  protected final lazy val allocatedWebServer: Allocated[IO, PekkoWebServer] =
    webServerResource.toAllocated.await(99.s)

  override def afterAll() =
    try
      Pekkos.shutDownHttpConnectionPools(system)
      allocatedWebServer.release.await(99.s)
    finally
      super.afterAll()

  protected final def requireAuthorizedAccess(client: PekkoHttpClient, expectedUserId: UserId = UserId("A-USER"))
    (implicit s: IO[Option[SessionToken]]): Unit =
    requireAccessToUnprotected(client)
    client.get_[String](Uri(s"$localUri/authorizedUser")).await(99.s) shouldEqual expectedUserId.string

  protected final def requireAccessIsUnauthorizedOrPublic(client: PekkoHttpClient)(implicit s: IO[Option[SessionToken]]): Unit =
    requireAccessToUnprotected(client)
    if isPublic then
      requireAccessIsPublic(client)
    else
      requireAccessIsUnauthorized(client)

  protected final def requireAccessIsPublic(client: PekkoHttpClient)(implicit s: IO[Option[SessionToken]]): Unit =
    assert(isPublic)
    requireAccessToUnprotected(client)
    getViaAuthorizedUsed(client)

  protected final def requireAccessIsUnauthorized(client: PekkoHttpClient)(implicit s: IO[Option[SessionToken]]): HttpException =
    requireAccessToUnprotected(client)
    val exception = intercept[PekkoHttpClient.HttpException]:
      getViaAuthorizedUsed(client)
    assert(exception.status == Unauthorized)
    assert(exception.header[`WWW-Authenticate`] ==
      Some(`WWW-Authenticate`(List(HttpChallenges.basic(realm = "TEST REALM")))))
    exception

  protected final def requireAccessIsForbidden(client: PekkoHttpClient)(implicit s: IO[Option[SessionToken]]): HttpException =
    requireAccessToUnprotected(client)
    val exception = intercept[PekkoHttpClient.HttpException]:
      getViaAuthorizedUsed(client)
    assert(exception.status == Forbidden)
    assert(exception.header[`WWW-Authenticate`].isEmpty)
    exception

  private def getViaAuthorizedUsed(client: PekkoHttpClient)(implicit s: IO[Option[SessionToken]]) =
    client.get_[String](Uri(s"$localUri/authorizedUser")).await(99.s)

  protected final def requireAccessToUnprotected(client: PekkoHttpClient)(implicit s: IO[Option[SessionToken]]): Unit =
    client.get_[String](Uri(s"$localUri/unprotected")).await(99.s) shouldEqual "THE RESPONSE"
