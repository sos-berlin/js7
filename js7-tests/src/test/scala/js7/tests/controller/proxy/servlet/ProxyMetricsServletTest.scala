package js7.tests.controller.proxy.servlet

import jakarta.servlet.http.HttpServletResponse.SC_OK
import java.net.URI
import java.net.http.HttpRequest
import java.net.http.HttpResponse.BodyHandlers
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.CatsUtils.Nel
import js7.common.metrics.MetricsProvider
import js7.common.pekkoutils.Pekkos
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.client.PekkoHttpControllerApi.admissionsToApiResource
import js7.proxy.data.GroupAndProxyId
import js7.proxy.servlets.ProxyMetricsServlet
import js7.proxy.{ControllerApi, ControllerApiRegister}
import js7.tests.testenv.ControllerAgentForScalaTest
import org.apache.pekko.actor.ActorSystem
import scala.jdk.OptionConverters.*

final class ProxyMetricsServletTest extends OurAsyncTestSuite, ControllerAgentForScalaTest:

  override def controllerConfig =
    config"""
      js7.auth.users.TEST-USER {
        permissions = [ ReadMetrics ]
      }
    """

  protected def agentPaths = Nil
  protected def items = Nil

  "test" in:
    val port = findFreeTcpPort()
    val controllerApiRegister =
      new ControllerApiRegister(Some(GroupAndProxyId.of("PROXY", "proxy")))(using ioRuntime) // normally provided by JProxyContext
    JettyWebServer.resource(
      "127.0.0.1", port, Seq(
        "/metrics" -> new ProxyMetricsServlet(() => Some(controllerApiRegister)))
    ).surround:
      OurHttpClient.resource.use: client =>
        val request = HttpRequest.newBuilder
          .uri:
            URI.create(s"http://127.0.0.1:$port/metrics?deep=true")
          .setHeader("Accept", MetricsProvider.PrometheusAcceptHeaderValue)
          .build()
        Pekkos.actorSystemResource("ProxyMetricsServletTest").use: actorSystem =>
          given ActorSystem = actorSystem
          ControllerApi.resource(
            admissionsToApiResource(Nel.one(controllerAdmission)),
            // Anchors ControllerApi as a singleton for ProxyMetricsServlet.
            register = Some(controllerApiRegister)
          ).use: controllerApi =>
            controllerApi.allowEngineMetrics(true) // Enable Engine metrics
              client.send(request, BodyHandlers.ofString)
      .map: response =>
        val body = response.body
        assert:
          response.statusCode() == SC_OK &
            response.headers().firstValue("Content-Type").toScala.contains(
              "text/plain; version=0.0.4;charset=utf-8") &
            body.contains(
            """java_lang_OperatingSystem_FreeMemorySize{js7ServerId="Proxy:proxy",js7ServerGroupId="Proxy:PROXY"""") &
            body.contains(
              """java_lang_OperatingSystem_FreeMemorySize{js7ServerId="Controller/primary",js7ServerGroupId="Engine:Controller"""")
