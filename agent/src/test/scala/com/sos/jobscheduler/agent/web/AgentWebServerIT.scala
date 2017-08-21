package com.sos.jobscheduler.agent.web

import akka.actor.ActorSystem
import com.google.inject.Guice
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.AgentConfiguration.InvalidAuthenticationDelay
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.test.AgentDirectoryProvider
import com.sos.jobscheduler.agent.views.{AgentOverview, AgentStartInformation}
import com.sos.jobscheduler.agent.web.AgentWebServerIT._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.scalautil.Closers.implicits.{RichClosersAny, RichClosersAutoCloseable}
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.sprayutils.WebServerBinding
import com.sos.jobscheduler.common.sprayutils.https.{Https, KeystoreReference}
import com.sos.jobscheduler.common.sprayutils.sprayclient.ExtendedPipelining.extendedSendReceive
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.time.timer.TimerServiceOverview
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPorts
import com.typesafe.config.ConfigFactory
import java.net.InetSocketAddress
import java.time.Instant.now
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag
import spray.can.Http.HostConnectorSetup
import spray.client.pipelining._
import spray.http.CacheDirectives.{`no-cache`, `no-store`}
import spray.http.HttpHeaders.{Accept, `Cache-Control`}
import spray.http.MediaTypes._
import spray.http.StatusCodes.{Forbidden, Unauthorized}
import spray.http.{BasicHttpCredentials, HttpRequest, HttpResponse, Uri}
import spray.httpx.SprayJsonSupport._
import spray.httpx.UnsuccessfulResponseException
import spray.httpx.encoding.Gzip
import spray.httpx.unmarshalling.{FromResponseUnmarshaller, PimpedHttpResponse}

/**
 * @author Joacim Zschimmer
 */
final class AgentWebServerIT extends FreeSpec with HasCloser with BeforeAndAfterAll with AgentDirectoryProvider {

  private lazy val List(httpPort, httpsPort) = findRandomFreeTcpPorts(2)
  private lazy val agentConfiguration = AgentConfiguration
    .forTest(Some(agentDirectory))
    .copy(http = Some(WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort))))
    .withHttpsInetSocketAddress(new InetSocketAddress("127.0.0.1", httpsPort))
    .finishAndProvideFiles
  private lazy val agent = RunningAgent(agentConfiguration) map { _.closeWithCloser } await 10.s
  private implicit lazy val actorSystem = {
    val config = ConfigFactory.parseMap(Map("spray.can.server.verbose-error-logging" → true))
    ActorSystem("AgentWebServerIT", config) withCloser { _.terminate() await 99.s }
  }
  private lazy val httpsSetup = Some(Https.toHostConnectorSetup(ClientKeystoreRef, agent.webServer.localHttpsUriOption.get))

  private def pipeline[A: FromResponseUnmarshaller](password: Option[String], setupOption: Option[HostConnectorSetup] = None): HttpRequest ⇒ Future[A] =
    (password map { o ⇒ addCredentials(BasicHttpCredentials("SHA512-USER", o)) } getOrElse identity[HttpRequest] _) ~>
    addHeader(Accept(`application/json`)) ~>
    addHeader(`Cache-Control`(`no-cache`, `no-store`)) ~>
    encode(Gzip) ~>
    extendedSendReceive(60.s.toFiniteDuration, setupOption) ~>
    decode(Gzip) ~>
    unmarshal[A]

  override protected def afterAll() = {
    close()
    super.afterAll()
  }

  "start" in {
    agent
  }

  "HTTPS" - {
    lazy val uri = s"${agent.webServer.localHttpsUriOption.get}/$Api"

    "Unauthorized request is rejected" - {
      "due to missing credentials" in {
        intercept[UnsuccessfulResponseException] {
          pipeline[HttpResponse](password = None, httpsSetup).apply(Get(s"$uri/task")) await 10.s
        }
        .response.status shouldEqual Unauthorized
      }

      "due to wrong credentials" in {
        val t = now
        val e = intercept[UnsuccessfulResponseException] {
          pipeline[AgentOverview](Some("WRONG-PASSWORD"), httpsSetup).apply(Get(uri)) await 10.s
        }
        assert(now - t > InvalidAuthenticationDelay - 50.ms)  // Allow for timer rounding
        e.response.status shouldEqual Unauthorized
      }

      addPostTextPlainText(uri)
    }

    "Authorized request" - {
      val password = Some("SHA512-PASSWORD")

      "is accepted" in {
        val overview = pipeline[AgentOverview](password, httpsSetup).apply(Get(uri)) await 10.s
        assert(overview.version == AgentStartInformation.VersionString)
      }

      addPostTextPlainText(uri, password)
    }

    addThroughputMeasurementTests(uri)
  }

  "HTTP" - {
    lazy val uri = s"${agent.webServer.localHttpUriOption.get}/$Api"

    "Without credentials" in {
      val overview = pipeline[AgentOverview](password = None).apply(Get(uri)) await 10.s
      assert(overview.version == AgentStartInformation.VersionString)
    }

    "Credentials are ignored" in {
      val overview = pipeline[AgentOverview](Some("WRONG-PASSWORD")).apply(Get(uri)) await 10.s
      assert(overview.version == AgentStartInformation.VersionString)
    }

    addPostTextPlainText(uri)
    addThroughputMeasurementTests(uri)
  }

  "WebService fails when HTTP port is not available" in {
    val webServer = Guice.createInjector(new AgentModule(agentConfiguration)).instance[AgentWebServer]
    intercept[RuntimeException] { webServer.start() await 10.s }
      .getMessage should include (s"127.0.0.1:$httpPort")
  }

  "close" in {
    agent.close()
  }

  private def addPostTextPlainText(uri: ⇒ Uri, password: Option[String] = None, setupOption: Option[HostConnectorSetup] = None): Unit =
    "POST plain/text is rejected due to CSRF" in {
      val response = intercept[UnsuccessfulResponseException] {
        pipeline[HttpResponse](password, setupOption).apply(Post(uri, "TEXT")) await 10.s
      } .response
      assert(response.status == Forbidden)
      assert(response.as[String].right.get == "HTML form POST is forbidden")
    }

  private def addThroughputMeasurementTests(uri: ⇒ Uri, setupOption: Option[HostConnectorSetup] = None): Unit = {
    if (false) {
      addThroughputMeasurementTest[TimerServiceOverview](s"$uri/timer", setupOption)
    }
  }

  private def addThroughputMeasurementTest[A: FromResponseUnmarshaller: ClassTag](uri: ⇒ Uri, setupOption: Option[HostConnectorSetup] = None) =
    s"Measure throughput of ${implicitClass[A].getSimpleName}" in {
      val get = Get(uri)
      val p = pipeline[A](Some("SHA512-PASSWORD"), setupOption)
      val m = 2 * sys.runtime.availableProcessors
      val n = 1000
      val result: A = p(get) await 10.s
      logger.info(s"${m * n} $result")
      val stopwatch = new Stopwatch
      (for (_ ← 1 to m) yield Future { for (_ ← 1 to n) p(get) await 10.s }) await 60.s
      logger.info(stopwatch.itemsPerSecondString(m * n, "requests"))
    }
}

private object AgentWebServerIT {
  private val Api = "jobscheduler/agent/api"
  private val ClientKeystoreRef = KeystoreReference(
    AgentDirectoryProvider.PublicHttpJksResource.url,
    Some(SecretString("jobscheduler")))
  private val logger = Logger(getClass)
}
