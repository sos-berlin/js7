package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.google.inject.Guice
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.inject.AgentModule
import com.sos.scheduler.engine.agent.test.AgentConfigDirectoryProvider
import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.AgentWebServerIT._
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.sprayutils.https.Https._
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPorts
import com.sos.scheduler.engine.common.utils.JavaResource
import java.net.InetSocketAddress
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.client.pipelining._
import spray.http.CacheDirectives.{`no-cache`, `no-store`}
import spray.http.HttpHeaders.{Accept, `Cache-Control`}
import spray.http.MediaTypes._
import spray.http.StatusCodes.Unauthorized
import spray.http.{BasicHttpCredentials, HttpRequest}
import spray.httpx.SprayJsonSupport._
import spray.httpx.UnsuccessfulResponseException
import spray.httpx.encoding.Gzip

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentWebServerIT extends FreeSpec with HasCloser with BeforeAndAfterAll with AgentConfigDirectoryProvider {

  private lazy val List(httpsPort, httpPort) = findRandomFreeTcpPorts(2)
  private lazy val agentConfiguration = AgentConfiguration
    .forTest(Some(dataDirectory))
    .copy(
      httpAddress = Some(new InetSocketAddress("127.0.0.1", httpPort)))
    .withHttpsInetSocketAddress(new InetSocketAddress("127.0.0.1", httpsPort))
  private lazy val webServer = Guice.createInjector(new AgentModule(agentConfiguration)).instance[AgentWebServer]
  private implicit lazy val actorSystem = ActorSystem("AgentWebServerIT") withCloser { _.shutdown() }

  private def pipeline(password: Option[String]): HttpRequest ⇒ Future[AgentOverview] =
    (password map { o ⇒ addCredentials(BasicHttpCredentials("SHA512-USER", o)) } getOrElse identity: RequestTransformer) ~>
    addHeader(Accept(`application/json`)) ~>
    addHeader(`Cache-Control`(`no-cache`, `no-store`)) ~>
    encode(Gzip) ~>
    sendReceive ~>
    decode(Gzip) ~>
    unmarshal[AgentOverview]

  override protected def beforeAll() = {
    acceptTlsCertificateFor(ClientKeystoreRef, webServer.localHttpsUriOption.get)
    super.beforeAll()
  }

  override protected def afterAll() = {
    close()
    super.afterAll()
  }

  "start" in {
    webServer.start() await 10.s
  }

  "HTTPS" - {
    lazy val uri = s"${webServer.localHttpsUriOption.get}/$OverviewPath"

    "Unauthorized due to missing credentials" in {
      intercept[UnsuccessfulResponseException] {
        pipeline(password = None)(Get(uri)) await 10.s
      }
      .response.status shouldEqual Unauthorized
    }

    "Unauthorized due to wrong credentials" in {
      intercept[UnsuccessfulResponseException] {
        pipeline(Some("WRONG-PASSWORD")).apply(Get(uri)) await 10.s
      }
      .response.status shouldEqual Unauthorized
    }

    "Authorized" in {
      val overview = pipeline(Some("SHA512-PASSWORD")).apply(Get(uri)) await 10.s
      assert(overview.totalTaskCount == 0)
    }
  }

  "HTTP" - {
    lazy val uri = s"${webServer.localHttpUriOption.get}/$OverviewPath"

    "Without credentials" in {
      val overview = pipeline(password = None)(Get(uri)) await 10.s
      assert(overview.totalTaskCount == 0)
    }

    "Credentials are ignored" in {
      val overview = pipeline(Some("WRONG-PASSWORD")).apply(Get(uri)) await 10.s
      assert(overview.totalTaskCount == 0)
    }
  }

  "WebService fails when HTTP port is not available" in {
    val webServer = Guice.createInjector(new AgentModule(agentConfiguration)).instance[AgentWebServer]
    intercept[RuntimeException] { webServer.start() await 10.s }
      .getMessage should include (s"127.0.0.1:$httpPort")
  }

  "close" in {
    webServer.close()
  }
}

private object AgentWebServerIT {
  private val OverviewPath = "jobscheduler/agent/api"
  private val ClientKeystoreRef = KeystoreReference(
    JavaResource("com/sos/scheduler/engine/agent/test/https.jks").url,
    Some(SecretString("jobscheduler")))
}
