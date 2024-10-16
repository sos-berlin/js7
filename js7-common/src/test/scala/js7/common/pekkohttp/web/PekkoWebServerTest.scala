package js7.common.pekkohttp.web

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, FiberIO, IO}
import fs2.Stream
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import javax.net.ssl.SSLHandshakeException
import js7.base.configutils.Configs.*
import js7.base.data.ByteArray
import js7.base.fs2utils.StreamExtensions.{interruptWhenF, prependOne}
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.Https.loadSSLContext
import js7.base.io.https.{KeyStoreRef, TrustStoreRef}
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.common.configuration.Js7Configuration
import js7.common.http.PekkoHttpUtils.*
import js7.common.http.StreamingSupport.asFs2Stream
import js7.common.pekkohttp.PekkoHttpServerUtils
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithStream
import js7.common.pekkohttp.web.PekkoWebServerTest.*
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.tester.ScalaTestUtils
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.impl.util.JavaVersion
import org.apache.pekko.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import org.apache.pekko.http.scaladsl.model.HttpMethods.GET
import org.apache.pekko.http.scaladsl.model.MediaTypes.`text/plain`
import org.apache.pekko.http.scaladsl.model.StatusCodes.OK
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.model.{ContentTypes, HttpRequest, MediaTypes}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.{ConnectionContext, Http}
import org.apache.pekko.util.ByteString
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class PekkoWebServerTest extends OurTestSuite, BeforeAndAfterAll:

  private given IORuntime = ioRuntime

  private implicit lazy val actorSystem: ActorSystem =
    newActorSystem("PekkoWebServerTest", executionContext = ioRuntime.compute)
  private lazy val List(httpPort, httpsPort) = findFreeTcpPorts(2)
  private lazy val directory = createTempDirectory("PekkoWebServerTest-")
  private lazy val http = Http()
  private val received = mutable.Buffer.empty[String]
  private val streamFiber = Deferred.unsafe[IO, FiberIO[Unit]]

  private lazy val keyStoreRef: KeyStoreRef =
    createDirectory(directory / "private")
    KeyStoreResource.copyToFile(directory / "private" / "https-keystore.p12")
    KeyStoreRef.fromSubconfig(
      config"""
          key-password = "jobscheduler"
          store-password = "jobscheduler"
          """,
      directory / "private/https-keystore.p12")
      .orThrow

  private lazy val webServer: Allocated[IO, PekkoWebServer] = PekkoWebServer
    .resource(
      Seq(
        WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort)),
        WebServerBinding.Https(new InetSocketAddress("127.0.0.1", httpsPort), keyStoreRef)),
      config"""
        # TODO Add test with client certificate
        js7.web.server.auth.https-client-authentication = off
        js7.web.server.shutdown-timeout = 10s
        js7.web.server.shutdown-delay = 500ms"""
        .withFallback(Js7Configuration.defaultConfig),
      toBoundRoute = routeBinding =>
        PekkoWebServer.BoundRoute.simple:
          path("TEST")(
            complete("OKAY")
          ) ~ path("STREAM"):
            completeWithStream(`text/plain(UTF-8)`):
              Stream
                .fixedRate[IO](100.ms)
                .as(ByteString(StreamingString))
                .prependOne(ByteString(StreamingStartedString))
                .interruptWhenF:
                  routeBinding
                    .whenStopRequested.get
                    .flatTap(deadline => IO:
                      logger.info(s"🟦 whenStopRequested ${deadline.elapsed.pretty}"))
                    .andWait(100.ms) // Pekko should await stream termination until `deadline`
                    .as(())
                .onFinalizeCase(exitCase => IO(logger.info(s"stream => $exitCase"))))
    .toAllocated
    .await(99.s)

  override def beforeAll() =
    super.beforeAll()
    webServer

  override def afterAll() =
    try
      webServer.release.await(99.s)
      Pekkos.terminateAndWait(actorSystem, 10.s)
      deleteDirectoryRecursively(directory)
    finally
      super.afterAll()

  "HTTP" in:
    val response = http.singleRequest(HttpRequest(GET, s"http://127.0.0.1:$httpPort/TEST"))
      .await(99.s)
    assert(response.status == OK)
    assert(response.utf8String.await(99.s) == "OKAY")

  "Start reading endless stream" in:
    val response = http
      .singleRequest:
        HttpRequest(
          GET, s"http://127.0.0.1:$httpPort/STREAM",
          Seq(Accept(`text/plain`)))
      .await(99.s)
    if response.status != OK then
      logger.info(s"response.utf8String=${response.entity.asUtf8String.await(9.s)}")
    assert(response.status == OK)

    response.entity.dataBytes.asFs2Stream()
      .map(_.utf8String)
      .foreach(string => IO:
        received += string
        logger.info(s"<--- ${ByteArray(string)}"))
      .compile
      .drain
      .start
      .flatTap(streamFiber.complete)
      .unsafeRunAndForget()
    streamFiber.get.await(9.s)
    awaitAndAssert:
      received.headOption.contains(StreamingStartedString)

  "HTTPS" - {
    "Client does not know server certificate" in:
      intercept[SSLHandshakeException]:
        http.singleRequest(HttpRequest(GET, s"https://127.0.0.1:$httpsPort/TEST"))
          .await(99.s)

    lazy val httpsConnectionContext =
      ConnectionContext.httpsClient(loadSSLContext(trustStoreRefs = Seq(ClientTrustStoreRef)))

    "Hostname verification rejects 127.0.0.1" in:
      val e = intercept[javax.net.ssl.SSLHandshakeException]:
        http
          .singleRequest(
            HttpRequest(GET, s"https://127.0.0.1:$httpsPort/TEST"),
            httpsConnectionContext)
          .await(99.s)
      if JavaVersion.majorVersion >= 23 then
        assert(e.getMessage == "(certificate_unknown) No subject alternative names matching IP address 127.0.0.1 found")
      else
        assert(e.getMessage == "No subject alternative names matching IP address 127.0.0.1 found"
            || e.getMessage == "General SSLEngine problem")

    "For this test, localhost must point to 127.0.0.1" in:
      // localhost must point to web servers's 127.0.0.1 (usually defined in /etc/host file).
      assert(InetAddress.getByName("localhost").getHostAddress == "127.0.0.1")

    "Hostname verification accepts localhost" in:
      val response = http
        .singleRequest(
          HttpRequest(GET, s"https://localhost:$httpsPort/TEST"),
          httpsConnectionContext)
        .await(99.s)
      assert(response.status == OK)
      assert(response.utf8String.await(99.s) == "OKAY")
  }

  "Stop web server" in:
    webServer.release.await(9.s)
    val outcome = streamFiber.get.flatMap(_.join).await(99.s)
    if !outcome.isSuccess then fail(outcome.toString)


object PekkoWebServerTest:

  private val logger = Logger[this.type]
  private val StreamingStartedString = String("STARTED\n")
  private val StreamingString = String("STREAMING\n")

  /* Following resources have been generated with the command line:
     js7-common/src/main/resources/js7/common/pekkohttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --alias=webserver \
        --distinguished-name="CN=web server, DC=PekkoWebServerTest, DC=tests, DC=js7, DC=sh" \
        --host=localhost \
        --config-directory=js7-common/src/test/resources/js7/common/pekkohttp/https/test-resources
   */
  private[web] val KeyStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/pekkohttp/https/test-resources/private/https-keystore.p12")
  private[web] val TrustStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/pekkohttp/https/test-resources/export/https-truststore.p12")

  private[web] val ClientTrustStoreRef = TrustStoreRef(
    TrustStoreResource.url,
    storePassword = SecretString("jobscheduler"))
