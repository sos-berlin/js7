package js7.tests.controller.proxy.servlet

import cats.effect.IO
import jakarta.servlet.http.HttpServletResponse.{SC_OK, SC_SERVICE_UNAVAILABLE}
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
import js7.data.proxy.ProxyId
import js7.proxy.ControllerApi
import js7.proxy.servlets.ProxyMetricsServlet
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
    JettyWebServer.resource("127.0.0.1", port, Seq("/metrics" -> new ProxyMetricsServlet)).surround:
      OurHttpClient.resource.use: client =>
        val request = HttpRequest.newBuilder
          .uri:
            URI.create(s"http://127.0.0.1:$port/metrics")
          .setHeader("Accept", MetricsProvider.PrometheusAcceptHeaderValue)
          .build()
        client.send(request, BodyHandlers.ofString).map: response =>
          assert:
            response.statusCode == SC_SERVICE_UNAVAILABLE &
              response.body == "No js7.proxy.ControllerApi with a ProxyId has been started\n"
        .productR:
          Pekkos.actorSystemResource("ProxyMetricsServletTest").use: actorSystem =>
            given ActorSystem = actorSystem
            ControllerApi.resource(
              admissionsToApiResource(Nel.one(controllerAdmission)),
              // Anchors ControllerApi as a singleton for ProxyMetricsServlet.
              // Don't do this in any other test when it could run concurrently !!!
              proxyId = Some(ProxyId("PROXY"))
            ).use: controllerApi =>
              controllerApi.setActive(true) // Enable Engine metrics
                client.send(request, BodyHandlers.ofString)
      .map: response =>
        val body = response.body
        assert:
          response.statusCode() == SC_OK &
            response.headers().firstValue("Content-Type").toScala.contains(
              "text/plain; version=0.0.4;charset=utf-8") &
            body.contains(
            """java_lang_OperatingSystem_FreeMemorySize{js7Server="Proxy:PROXY"}""") &
            body.contains(
              """java_lang_OperatingSystem_FreeMemorySize{js7Server="Controller/primary"}""")
