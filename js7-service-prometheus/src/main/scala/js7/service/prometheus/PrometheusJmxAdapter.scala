package js7.service.prometheus

import io.prometheus.jmx.JmxCollector
import io.prometheus.metrics.exporter.common.PrometheusScrapeHandler
import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import java.io.{ByteArrayOutputStream, FileNotFoundException, IOException}
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.service.prometheus.PrometheusJmxAdapter.*
import org.apache.pekko.http.scaladsl.model.{HttpMethods, HttpRequest}
import org.apache.pekko.util.ByteString

final class PrometheusJmxAdapter(configDir: Option[Path] = None):

  private val registry = new PrometheusRegistry
  private val jmxCollector =
    val yamlConfig =
      configDir.fold(DefaultYamlConfig): configDir =>
        val file = configDir / "prometheus.yaml"
        try file.contentString
        catch
          case _: FileNotFoundException => DefaultYamlConfig
          case e: IOException =>
            logger.warn(s"$file: $e • Using default configuration")
            DefaultYamlConfig
    new JmxCollector(yamlConfig)
  jmxCollector.register(registry)

  private val scrapeHandler = new PrometheusScrapeHandler(registry)
  private val textWriter = PrometheusTextFormatWriter.builder().build()

  def metricsByteString(): ByteString =
    meter:
      val outputStream = new ByteArrayOutputStream(256*1024)
      val snapshots = registry.scrape()
      textWriter.write(outputStream, snapshots)
      ByteString.fromArrayUnsafe(outputStream.toByteArray)

  // Not used !!!
  private def metricsByteString(request: HttpRequest = HttpRequest(HttpMethods.GET, "/metrics")): ByteString =
    val exchange = new PrometheusJmxAdapterForHttp.PekkoPromtheusExchange(request)
    scrapeHandler.handleRequest(exchange)
    exchange.metricsByteString


object PrometheusJmxAdapter:
  private val logger = Logger[this.type]
  private val meter = CallMeter("PrometheusJmxAdapter")
  private val DefaultYamlConfig = """
   |excludeObjectNames: ["cats.effect.*:type=*"]
   |""".stripMargin
