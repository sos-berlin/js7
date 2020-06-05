package js7.common.akkahttp.web

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.common.akkahttp.https.{AkkaHttps, KeyStoreRef, TrustStoreRef}
import js7.common.akkahttp.web.AkkaWebServer.HasUri
import js7.common.akkahttp.web.AkkaWebServerTest._
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.common.utils.JavaResource
import com.typesafe.config.ConfigFactory
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import javax.net.ssl.SSLHandshakeException
import monix.execution.Scheduler
import org.scalatest.BeforeAndAfterAll
import Scheduler.Implicits.global
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec
import js7.common.scalautil.MonixUtils.syntax._

/**
  * @author Joacim Zschimmer
  */
final class AkkaWebServerTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private implicit lazy val actorSystem = newActorSystem("AkkaWebServerTest")
  private lazy val List(httpPort, httpsPort) = findFreeTcpPorts(2)
  private lazy val directory = createTempDirectory("AkkaWebServerTest-")
  private implicit lazy val materializer = ActorMaterializer()
  private lazy val http = Http()

  private lazy val webServer = new AkkaWebServer with HasUri {
    protected val config = ConfigFactory.parseString("jobscheduler.webserver.shutdown-timeout = 10s")
    protected def actorSystem = AkkaWebServerTest.this.actorSystem
    protected def scheduler = Scheduler.global

    private val keyStoreRef: KeyStoreRef = {
      createDirectory(directory / "private")
      KeyStoreResource copyToFile directory / "private" / "https-keystore.p12"
      KeyStoreRef.fromConfig(
        ConfigFactory.parseString(
          """jobscheduler.https.keystore {
            |  key-password = jobscheduler
            |  store-password = jobscheduler
            |}
            |""".stripMargin),
        directory / "private/https-keystore.p12")
      .orThrow
    }

    val bindings =
      WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort)) ::
      WebServerBinding.Https(new InetSocketAddress("127.0.0.1", httpsPort), keyStoreRef, mutual = false) :: Nil

    def newRoute(binding: WebServerBinding) = AkkaWebServer.BoundRoute(
      path("TEST") {
        complete("OKAY")
      })
  }

  override def beforeAll(): Unit = {
    webServer.start() await 99.seconds
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    actorSystem.terminate()
    deleteDirectoryRecursively(directory)
    super.afterAll()
  }

  "HTTP" in {
    val response = http.singleRequest(HttpRequest(GET, s"http://127.0.0.1:$httpPort/TEST")) await 99.seconds
    assert(response.status == StatusCodes.OK)
    assert(response.utf8StringFuture.await(99.seconds) == "OKAY")
  }

  "HTTPS" - {
    "Client does not know server certificate" in {
      intercept[SSLHandshakeException] {
        http.singleRequest(HttpRequest(GET, s"https://127.0.0.1:$httpsPort/TEST")) await 99.seconds }
    }

    lazy val httpsConnectionContext = AkkaHttps.loadHttpsConnectionContext(trustStoreRef = Some(ClientTrustStoreRef))

    "Hostname verification rejects 127.0.0.1" in {
      val e = intercept[akka.stream.ConnectionException] {
        http.singleRequest(HttpRequest(GET, s"https://127.0.0.1:$httpsPort/TEST"), httpsConnectionContext) await 99.seconds
      }
      assert(e.getMessage == "Hostname verification failed! Expected session to be for 127.0.0.1")
    }

    "Hostname verification accepts localhost" in {
      assert(InetAddress.getByName("localhost").getHostAddress == "127.0.0.1")  // Check file /etc/host
      val response = http.singleRequest(HttpRequest(GET, s"https://localhost:$httpsPort/TEST"), httpsConnectionContext) await 99.seconds
      assert(response.status == StatusCodes.OK)
      assert(response.utf8StringFuture.await(99.seconds) == "OKAY")
    }
  }
}

object AkkaWebServerTest {
  // Following resources have been generated with the command line:
  // common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=test -config-directory=common/src/test/resources/js7/common/akkahttp/https/config
  private val KeyStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/akkahttp/https/config/private/https-keystore.p12")
  private val TrustStoreResource = JavaResource(getClass.getClassLoader,
    "js7/common/akkahttp/https/config/export/https-truststore.p12")

  private val ClientTrustStoreRef = TrustStoreRef(TrustStoreResource.url, storePassword = SecretString("jobscheduler"))
}
