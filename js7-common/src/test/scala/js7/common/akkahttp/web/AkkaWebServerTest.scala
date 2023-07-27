package js7.common.akkahttp.web

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.{ConnectionContext, Http}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import javax.net.ssl.SSLHandshakeException
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.https.Https.loadSSLContext
import js7.base.io.https.{KeyStoreRef, TrustStoreRef}
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.common.akkahttp.web.AkkaWebServerTest.*
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.configuration.Js7Configuration
import js7.common.http.AkkaHttpUtils.*
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.BeforeAndAfterAll

/**
  * @author Joacim Zschimmer
  */
final class AkkaWebServerTest extends OurTestSuite with BeforeAndAfterAll
{
  private implicit lazy val actorSystem: ActorSystem =
    newActorSystem("AkkaWebServerTest")
  private lazy val List(httpPort, httpsPort) = findFreeTcpPorts(2)
  private lazy val directory = createTempDirectory("AkkaWebServerTest-")
  private lazy val http = Http()

  private lazy val keyStoreRef: KeyStoreRef = {
    createDirectory(directory / "private")
    KeyStoreResource.copyToFile(directory / "private" / "https-keystore.p12")
    KeyStoreRef.fromSubconfig(
      config"""
          key-password = "jobscheduler"
          store-password = "jobscheduler"
          """,
      directory / "private/https-keystore.p12")
      .orThrow
  }

  private lazy val webServer: Allocated[Task, AkkaWebServer] = AkkaWebServer
    .resource(
      Seq(
        WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort)),
        WebServerBinding.Https(new InetSocketAddress("127.0.0.1", httpsPort), keyStoreRef)),
      config"""
        # TODO Add test with client certificate
        js7.web.server.auth.https-client-authentication = off
        js7.web.server.shutdown-timeout = 10s"""
        .withFallback(Js7Configuration.defaultConfig),
      toBoundRoute = routeBinding =>
        AkkaWebServer.BoundRoute.simple(
          path("TEST") {
            complete("OKAY")
          }))
    .toAllocated
    .await(99.s)

  override def beforeAll() = {
    webServer
    super.beforeAll()
  }

  override def afterAll() = {
    webServer.release.await(99.s)
    Akkas.terminateAndWait(actorSystem, 10.s)
    deleteDirectoryRecursively(directory)
    super.afterAll()
  }

  "HTTP" in {
    val response = http.singleRequest(HttpRequest(GET, s"http://127.0.0.1:$httpPort/TEST"))
      .await(99.s)
    assert(response.status == OK)
    assert(response.utf8String.await(99.s) == "OKAY")
  }

  "HTTPS" - {
    "Client does not know server certificate" in {
      intercept[SSLHandshakeException] {
        http.singleRequest(HttpRequest(GET, s"https://127.0.0.1:$httpsPort/TEST"))
          .await(99.s)
      }
    }

    lazy val httpsConnectionContext =
      ConnectionContext.httpsClient(loadSSLContext(trustStoreRefs = Seq(ClientTrustStoreRef)))

    "Hostname verification rejects 127.0.0.1" in {
      val e = intercept[javax.net.ssl.SSLHandshakeException] {
        http
          .singleRequest(
            HttpRequest(GET, s"https://127.0.0.1:$httpsPort/TEST"),
            httpsConnectionContext)
          .await(99.s)
      }
      assert(e.getMessage == "No subject alternative names matching IP address 127.0.0.1 found" ||
             e.getMessage == "General SSLEngine problem")
    }

    "For this test, localhost must point to 127.0.0.1" in {
      // localhost must point to web servers's 127.0.0.1 (usually defined in /etc/host file).
      assert(InetAddress.getByName("localhost").getHostAddress == "127.0.0.1")
    }

    "Hostname verification accepts localhost" in {
      val response = http
        .singleRequest(
          HttpRequest(GET, s"https://localhost:$httpsPort/TEST"),
          httpsConnectionContext)
        .await(99.s)
      assert(response.status == OK)
      assert(response.utf8String.await(99.s) == "OKAY")
    }
  }
}

object AkkaWebServerTest
{
  /* Following resources have been generated with the command line:
     js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --alias=webserver \
        --distinguished-name="CN=web server, DC=AkkaWebServerTest, DC=tests, DC=js7, DC=sh" \
        --host=localhost \
        --config-directory=js7-common/src/test/resources/js7/common/akkahttp/https/test-resources
   */
  private[web] val KeyStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/akkahttp/https/test-resources/private/https-keystore.p12")
  private[web] val TrustStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/akkahttp/https/test-resources/export/https-truststore.p12")

  private[web] val ClientTrustStoreRef = TrustStoreRef(
    TrustStoreResource.url,
    storePassword = SecretString("jobscheduler"))
}
