package js7.common.pekkohttp.web

import cats.syntax.flatMap.*
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import js7.base.configutils.Configs.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.toAllByteSequenceOps
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.Https.loadSSLContext
import js7.base.io.https.{KeyStoreRef, TrustStoreRef}
import js7.base.problem.Checked.Ops
import js7.base.test.{OurTestSuite}
import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.common.configuration.Js7Configuration
import js7.common.http.PekkoHttpUtils.*
import js7.common.pekkohttp.web.PekkoWebServerHttpsChangeTest.*
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.HttpMethods.GET
import org.apache.pekko.http.scaladsl.model.StatusCodes.OK
import org.apache.pekko.http.scaladsl.model.{HttpRequest, HttpResponse}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.{ConnectionContext, Http}
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.util.{Failure, Try}

final class PekkoWebServerHttpsChangeTest extends OurTestSuite, BeforeAndAfterAll
{
  private given IORuntime = ioRuntime

  private implicit lazy val actorSystem: ActorSystem =
    newActorSystem("PekkoWebServerHttpsChangeTest", executionContext = ioRuntime.compute)
  private lazy val List(httpPort, httpsPort) = findFreeTcpPorts(2)
  private lazy val directory = createTempDirectory("PekkoWebServerHttpsChangeTest-")
  private lazy val http = Http()

  private lazy val certFile = directory / "private" / "https-keystore.p12"

  private lazy val keyStoreRef: KeyStoreRef = {
    createDirectory(directory / "private")
    PekkoWebServerTest.KeyStoreResource.copyToFile(certFile)
    KeyStoreRef.fromSubconfig(
      config"""
          key-password = "jobscheduler"
          store-password = "jobscheduler"
          """,
      directory / "private/https-keystore.p12")
      .orThrow
  }

  private implicit val testEventBus: StandardEventBus[Any] = new StandardEventBus[Any]

  private lazy val webServer: Allocated[IO, PekkoWebServer] = PekkoWebServer
    .resource(
      Seq(
        WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort)),
        WebServerBinding.Https(new InetSocketAddress("127.0.0.1", httpsPort), keyStoreRef)),
      config"""
        js7.web.server.auth.https-client-authentication = off
        js7.web.server.shutdown-timeout = 10s
        js7.web.server.shutdown-delay = 500ms
        js7.directory-watcher.watch-delay = 10ms
        js7.directory-watcher.directory-silence = 10ms
        """.withFallback(Js7Configuration.defaultConfig),
      toBoundRoute = routeBinding =>
        PekkoWebServer.BoundRoute.simple(
          path("TEST") {
            complete(s"OKAY-${routeBinding.revision}")
          }))
    .flatTap(_.restartWhenHttpsChanges)
    .toAllocated
    .await(99.s)

  override def beforeAll() =
    super.beforeAll()
    webServer

  override def afterAll() =
    webServer.release.await(99.s)
    Pekkos.terminateAndWait(actorSystem, 10.s)
    deleteDirectoryRecursively(directory)
    super.afterAll()

  "HTTP" in {
    val response = http.singleRequest(HttpRequest(GET, s"http://127.0.0.1:$httpPort/TEST"))
      .await(99.s)
    assert(response.status == OK)
    assert(response.utf8String.await(99.s) == "OKAY-1")
  }

  "For this test, localhost must point to 127.0.0.1" in {
    // localhost must point to web servers's 127.0.0.1 (usually defined in /etc/host file).
    assert(InetAddress.getByName("localhost").getHostAddress == "127.0.0.1")
  }

  "HTTPS, unchanged key" in {
    val httpsConnectionContext1 = ConnectionContext.httpsClient(
      loadSSLContext(trustStoreRefs = Seq(PekkoWebServerTest.ClientTrustStoreRef)))

    val response = http
      .singleRequest(
        HttpRequest(GET, s"https://localhost:$httpsPort/TEST"),
        httpsConnectionContext1)
      .await(99.s)
    assert(response.status == OK)
    assert(response.utf8String.await(99.s) == "OKAY-1")
  }

  "HTTPS, change client's trust certificate, then change server's key" - {
    lazy val changedHttpsConnectionContext = ConnectionContext.httpsClient(
      loadSSLContext(trustStoreRefs = Seq(ChangedClientTrustStoreRef)))

    "Updated client does not match old server certificate" in {
      val e = intercept[javax.net.ssl.SSLHandshakeException] {
        http
          .singleRequest(
            HttpRequest(GET, s"https://localhost:$httpsPort/TEST"),
            changedHttpsConnectionContext)
          .await(99.s)
      }
      assert(e.getMessage == "No trusted certificate found")
    }

    val changedCert = ChangedKeyStoreResource.readAs[ByteArray]
    var writtenLength = 0
    lazy val fileChanged = Promise[Unit]()
    lazy val restarted = Promise[Unit]()

    "Write some bytes of server certificate" in {
      fileChanged
      testEventBus.subscribe[PekkoWebServer.BeforeRestartEvent.type](_ =>
        fileChanged.trySuccess(()))

      restarted
      testEventBus.subscribe[PekkoWebServer.RestartedEvent.type](_ =>
        restarted.trySuccess(()))

      writtenLength = 3
      certFile := changedCert.take(writtenLength)
      fileChanged.future.await(99.s)
      sleep(3.s)

      assert(!restarted.future.isCompleted)
    }

    "Write remaining of server certificate" in {
      if writtenLength == 0 then {
        certFile := changedCert
      } else {
        certFile ++= changedCert.drop(writtenLength)
      }
      restarted.future.await(99.s)

      // Due to the second changed, the SinglePortPekkoWebServer is being restarted twice:
      // 1) due to AutoRestartableService
      // 2) due to seconds file change event
      // Not easy to detect that the second restart is not needed. But it's an unusual case.
      val until = now + 99.s
      var tried: Try[HttpResponse] = Failure(new RuntimeException("??"))
      while now < until && tried.isFailure do {
        tried = Try(http
          .singleRequest(
            HttpRequest(GET, s"https://localhost:$httpsPort/TEST"),
            changedHttpsConnectionContext)
          .await(99.s))
      }
      val response = tried.get
      assert(response.status == OK)
      assert(Set("OKAY-2", "OKAY-3")(response.utf8String.await(99.s)))
    }
  }
}


object PekkoWebServerHttpsChangeTest
{
  /* Following resources have been generated with the command line:
     js7-common/src/main/resources/js7/common/pekkohttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --alias=webserver \
        --distinguished-name="CN=web server, DC=PekkoWebServerHttpsChangeTest, DC=tests, DC=js7, DC=sh" \
        --host=localhost \
        --config-directory=js7-common/src/test/resources/js7/common/pekkohttp/https/test2-resources
   */
  private val ChangedKeyStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/pekkohttp/https/test2-resources/private/https-keystore.p12")
  private val ChangedTrustStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/pekkohttp/https/test2-resources/export/https-truststore.p12")

  private val ChangedClientTrustStoreRef = TrustStoreRef(
    ChangedTrustStoreResource.url,
    storePassword = SecretString("jobscheduler"))
}
