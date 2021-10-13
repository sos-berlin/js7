package js7.common.akkahttp.web

import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.{ConnectionContext, Http}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import javax.net.ssl.SSLHandshakeException
import js7.base.configutils.Configs._
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.https.Https.loadSSLContext
import js7.base.io.https.{KeyStoreRef, TrustStoreRef}
import js7.base.problem.Checked.Ops
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.common.akkahttp.web.AkkaWebServer.HasUri
import js7.common.akkahttp.web.AkkaWebServerTest._
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AkkaWebServerTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private implicit lazy val actorSystem = newActorSystem("AkkaWebServerTest")
  private lazy val List(httpPort, httpsPort) = findFreeTcpPorts(2)
  private lazy val directory = createTempDirectory("AkkaWebServerTest-")
  private lazy val http = Http()

  private lazy val webServer = new AkkaWebServer with HasUri {
    // TODO Add test with client certificate
    protected val config = config"""
      js7.web.server.auth.https-client-authentication = off
      js7.web.server.shutdown-timeout = 10s"""
    protected def actorSystem = AkkaWebServerTest.this.actorSystem

    private val keyStoreRef: KeyStoreRef = {
      createDirectory(directory / "private")
      KeyStoreResource copyToFile directory / "private" / "https-keystore.p12"
      KeyStoreRef.fromConfig(
        config"""
          js7.web.https.keystore {
            key-password = "jobscheduler"
            store-password = "jobscheduler"
          }""",
        directory / "private/https-keystore.p12")
      .orThrow
    }

    val bindings =
      WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort)) ::
      WebServerBinding.Https(new InetSocketAddress("127.0.0.1", httpsPort), keyStoreRef) :: Nil

    def newRoute(binding: WebServerBinding, whenTerminating: Future[Deadline]) =
      AkkaWebServer.BoundRoute(
        path("TEST") {
          complete("OKAY")
        },
        whenTerminating)
  }

  override def beforeAll(): Unit = {
    webServer.start() await 99.s
    super.beforeAll()
  }

  override def afterAll(): Unit = {
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
      ConnectionContext.httpsClient(loadSSLContext(trustStoreRefs = ClientTrustStoreRef :: Nil))

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

    "Hostname verification accepts localhost" in {
      // localhost must point to web servers's 127.0.0.1 (usually defined in /etc/host file).
      assert(InetAddress.getByName("localhost").getHostAddress == "127.0.0.1")

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
  private val KeyStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/akkahttp/https/test-resources/private/https-keystore.p12")
  private val TrustStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/akkahttp/https/test-resources/export/https-truststore.p12")

  private val ClientTrustStoreRef = TrustStoreRef(
    TrustStoreResource.url,
    storePassword = SecretString("jobscheduler"))
}
