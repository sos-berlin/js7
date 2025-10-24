package js7.service.prometheus

import io.prometheus.jmx.JmxCollector
import io.prometheus.metrics.exporter.common.PrometheusScrapeHandler
import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{NoSuchFileException, Path}
import js7.base.data.ByteSeqOutputStream
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.service.prometheus.PrometheusJmxAdapter.*
import org.apache.pekko.http.scaladsl.model.{HttpMethods, HttpRequest}
import org.apache.pekko.util.ByteString
import org.jetbrains.annotations.TestOnly

private[prometheus] final class PrometheusJmxAdapter(configDir: Option[Path] = None):

  private val registry = new PrometheusRegistry
  private val jmxCollector =
    new JmxCollector(
      configDir.fold(DefaultYamlConfig): configDir =>
        val file = configDir / "prometheus.yaml"
        try file.contentString
        catch
          case _: NoSuchFileException => DefaultYamlConfig
          case e: IOException =>
            logger.warn(s"$file: $e • Using default configuration")
            DefaultYamlConfig)

  jmxCollector.register(registry)

  private val scrapeHandler = new PrometheusScrapeHandler(registry)
  private val textWriter = PrometheusTextFormatWriter.builder().build()
  private var lastSize = 128 * 1024

  def metricsByteString(): ByteString =
    meter:
      val outputStream = new ByteSeqOutputStream(lastSize + lastSize / 5)
      outputStream.writeBytes(HeadlineBytes)
      textWriter.write(outputStream, registry.scrape())
      lastSize = outputStream.size()
      outputStream.byteSeq[ByteString]

  // Not used !!!
  private def metricsByteString(request: HttpRequest = HttpRequest(HttpMethods.GET, "/metrics"))
  : ByteString =
    val exchange = new PrometheusJmxAdapterForHttp.PekkoPromtheusExchange(request)
    scrapeHandler.handleRequest(exchange)
    exchange.metricsByteString


private[prometheus] object PrometheusJmxAdapter:
  private val logger = Logger[this.type]
  private val meter = CallMeter("PrometheusJmxAdapter")
  private val DefaultYamlConfig = """
   |excludeObjectNames: ["cats.effect.*:type=*"]
   |""".stripMargin

  private val HeadlineBytes =
    s"""### Experimental metrics for Prometheus ###
       |
       |""".stripMargin.getBytes(UTF_8)

  @TestOnly
  val Headline = new String(HeadlineBytes, UTF_8)
