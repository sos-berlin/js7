package com.sos.jobscheduler.common.akkahttp.web

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeyStoreRef}
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer.HasUri
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServerTest._
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.akkautils.Akkas
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPorts
import com.sos.jobscheduler.common.utils.JavaResource
import com.typesafe.config.ConfigFactory
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.Files.{createDirectory, createTempDirectory}
import javax.net.ssl.SSLHandshakeException
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AkkaWebServerTest extends FreeSpec with BeforeAndAfterAll
{
  private implicit lazy val actorSystem = Akkas.newActorSystem("AkkaWebServerTest")
  private lazy val List(httpPort, httpsPort) = findRandomFreeTcpPorts(2)
  private lazy val directory = createTempDirectory("AkkaWebServerTest-")
  private implicit lazy val materializer = ActorMaterializer()
  private lazy val http = Http()

  private lazy val webServer = new AkkaWebServer with HasUri {
    implicit def actorSystem = AkkaWebServerTest.this.actorSystem
    implicit def executionContext = ExecutionContext.global

    private val keyStoreRef: KeyStoreRef = {
      createDirectory(directory / "private")
      KeyStoreResource copyToFile directory / "private" / "https-keystore.p12"
      KeyStoreRef.fromConfig(
        ConfigFactory.parseString(
          """jobscheduler.webserver.https.keystore {
            |  key-password = jobscheduler
            |  store-password = jobscheduler
            |}""".stripMargin),
        directory / "private/https-keystore.p12")
      .orThrow
    }

    val bindings =
      WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort)) ::
      WebServerBinding.Https(new InetSocketAddress("127.0.0.1", httpsPort), keyStoreRef, mutual = false) :: Nil

    def newRoute(binding: WebServerBinding) =
      path("TEST") {
        complete("OKAY")
      }
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
    assert(response.utf8StringFuture.await(9.seconds) == "OKAY")
  }

  "HTTPS" - {
    "Client does not know server certificate" in {
      intercept[SSLHandshakeException] {
        http.singleRequest(HttpRequest(GET, s"https://127.0.0.1:$httpsPort/TEST")) await 99.seconds }
    }

    lazy val httpsConnectionContext = Https.toHttpsConnectionContext(ClientKeyStoreRef)

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
      assert(response.utf8StringFuture.await(9.seconds) == "OKAY")
    }
  }
}

object AkkaWebServerTest {
  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=test -config-directory=common/src/test/resources/com/sos/jobscheduler/common/akkahttp/https/config
  private val KeyStoreResource = JavaResource("com/sos/jobscheduler/common/akkahttp/https/config/private/https-keystore.p12")

  private val ClientKeyStoreRef = KeyStoreRef(KeyStoreResource.url, storePassword = SecretString("jobscheduler"))
}
