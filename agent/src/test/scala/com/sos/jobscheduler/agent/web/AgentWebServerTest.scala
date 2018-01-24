package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, Authorization, BasicHttpCredentials, `Cache-Control`}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, StatusCode, Uri}
import akka.http.scaladsl.unmarshalling.FromResponseUnmarshaller
import akka.http.scaladsl.{Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer
import com.google.inject.Guice
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.AgentConfiguration.InvalidAuthenticationDelay
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider
import com.sos.jobscheduler.agent.views.{AgentOverview, AgentStartInformation}
import com.sos.jobscheduler.agent.web.AgentWebServerTest._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.common.CirceJsonSupport._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpClientUtils.RichHttpResponse
import com.sos.jobscheduler.common.akkahttp.WebServerBinding
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeystoreReference}
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.Futures.blockingFuture
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.time.timer.TimerServiceOverview
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPorts
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.time.Instant.now
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
final class AgentWebServerTest extends FreeSpec with HasCloser with BeforeAndAfterAll with TestAgentDirectoryProvider {

  private lazy val List(httpPort, httpsPort) = findRandomFreeTcpPorts(2)
  private lazy val agentConfiguration = AgentConfiguration
    .forTest(Some(agentDirectory))
    .copy(http = Some(WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort))))
    .withHttpsInetSocketAddress(new InetSocketAddress("127.0.0.1", httpsPort))
  private lazy val agent = RunningAgent(agentConfiguration) await 10.s
  private implicit lazy val actorSystem = {
    val config = ConfigFactory.parseMap(Map("akka.http.server.verbose-error-messages" → true).asJava)
    ActorSystem("AgentWebServerTest", config) withCloser { _.terminate() await 99.s }
  }
  private implicit lazy val materializer = ActorMaterializer()
  private lazy val http = Http()

  override protected def afterAll() = {
    close()
    super.afterAll()
  }

  "start" in {
    agent
  }

  "HTTPS" - {
    implicit lazy val httpsConnectionContext = Https.toHttpsConnectionContext(ClientKeystoreRef)
    lazy val uri = s"${agent.webServer.localHttpsUriOption.get}/$Api"

    "Unauthorized request is rejected" - {
      "due to missing credentials" in {
        intercept[HttpException] {
          executeRequest[HttpResponse](HttpRequest(GET, s"$uri/task"), password = None) await 99.s
        }
        .status shouldEqual Unauthorized
      }

      "due to wrong credentials" in {
        val t = now
        val e = intercept[HttpException] {
          executeRequest[AgentOverview](HttpRequest(GET, uri), Some("WRONG-PASSWORD")) await 10.s
        }
        assert(now - t > InvalidAuthenticationDelay - 50.ms)  // Allow for timer rounding
        e.status shouldEqual Unauthorized
      }

      addPostTextPlainText(uri)
    }

    "Authorized request" - {
      val password = Some("SHA512-PASSWORD")

      "is accepted" in {
        val overview = executeRequest[AgentOverview](HttpRequest(GET, uri), password) await 10.s
        assert(overview.version == AgentStartInformation.VersionString)
      }

      addPostTextPlainText(uri, password)
    }

    addThroughputMeasurementTests(uri)
  }

  "HTTP" - {
    lazy val uri = s"${agent.webServer.localHttpUriOption.get}/$Api"

    "Without credentials" in {
      val overview = executeRequest[AgentOverview](HttpRequest(GET, uri), password = None) await 10.s
      assert(overview.version == AgentStartInformation.VersionString)
    }

    "Credentials are ignored" in {
      val overview = executeRequest[AgentOverview](HttpRequest(GET, uri), Some("WRONG-PASSWORD")) await 10.s
      assert(overview.version == AgentStartInformation.VersionString)
    }

    addPostTextPlainText(uri)
    addThroughputMeasurementTests(uri)
  }

  "WebService fails when HTTP port is not available" in {
    val webServer = Guice.createInjector(new AgentModule(agentConfiguration)).instance[AgentWebServer]
    intercept[Exception] { webServer.start() await 10.s }
      .getMessage should include ("Bind failed because of Address already in use")  //(s"127.0.0.1:$httpPort")
  }

  "close" in {
    agent.close()
  }

  private def addPostTextPlainText(uri: ⇒ Uri, password: Option[String] = None)(implicit httpsConnectionContext: HttpsConnectionContext = http.defaultClientHttpsContext): Unit =
    "POST plain/text is rejected due to CSRF" in {
      val exception = intercept[HttpException] {
        executeRequest[HttpResponse](HttpRequest(POST, uri, entity = "TEXT"), password) await 10.s
      }
      assert(exception.status == Forbidden)
      assert(exception.getMessage contains "HTML form POST is forbidden")
    }

  private def addThroughputMeasurementTests(uri: ⇒ Uri)(implicit httpsConnectionContext: HttpsConnectionContext = http.defaultClientHttpsContext): Unit = {
    if (false) {
      addThroughputMeasurementTest[TimerServiceOverview](s"$uri/timer")
    }
  }

  private def addThroughputMeasurementTest[A: FromResponseUnmarshaller: ClassTag](uri: ⇒ Uri)(implicit httpsConnectionContext: HttpsConnectionContext = http.defaultClientHttpsContext) =
    s"Measure throughput of ${implicitClass[A].getSimpleName}" in {
      val get = HttpRequest(GET, uri)
      val m = 2 * sys.runtime.availableProcessors
      val n = 1000
      val result: A = executeRequest[A](get, Some("SHA512-PASSWORD")) await 10.s
      logger.info(s"${m * n} $result")
      val stopwatch = new Stopwatch
      (for (_ ← 1 to m) yield blockingFuture { for (_ ← 1 to n) executeRequest[A](get, Some("SHA512-PASSWORD")) await 10.s }) await 60.s
      logger.info(stopwatch.itemsPerSecondString(m * n, "requests"))
    }

  private def executeRequest[A: FromResponseUnmarshaller](request: HttpRequest, password: Option[String])(implicit httpsConnectionContext: HttpsConnectionContext = http.defaultClientHttpsContext): Future[A] = {
    val whenHttpResponse = http.singleRequest(
      Gzip.encodeMessage(request withHeaders
        Accept(`application/json`) ::
        `Cache-Control`(`no-cache`, `no-store`) ::
        (password.toList map { pw ⇒ Authorization(BasicHttpCredentials("SHA512-USER", pw)) }) ++
        request.headers),
      httpsConnectionContext)
    whenHttpResponse map { o ⇒ Gzip.decodeMessage(o) } flatMap { response ⇒
      if (response.status.isSuccess)
        implicitly[FromResponseUnmarshaller[A]].apply(response)
      else
        throw new HttpException(response.status, response.utf8StringFuture await 99.s)
    }
  }
}

private object AgentWebServerTest {
  private val Api = "agent/api"
  private val ClientKeystoreRef = KeystoreReference(
    TestAgentDirectoryProvider.PublicHttpJksResource.url,
    Some(SecretString("jobscheduler")))
  private val logger = Logger(getClass)

  private class HttpException(val status: StatusCode, message: String) extends RuntimeException(s"$status: $message".trim)
}
