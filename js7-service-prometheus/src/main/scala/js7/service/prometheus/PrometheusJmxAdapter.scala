package js7.service.prometheus

import io.prometheus.jmx.JmxCollector
import io.prometheus.metrics.exporter.common.PrometheusScrapeHandler
import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.{PrometheusRegistry, PrometheusScrapeRequest}
import java.io.IOException
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{NoSuchFileException, Path}
import javax.management.ObjectName
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteSeqOutputStream, ByteSequence}
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.base.utils.ScalaUtils.syntax.foreachWithBracket
import js7.service.prometheus.PrometheusJmxAdapter.*
import org.apache.pekko.http.scaladsl.model.{HttpMethods, HttpRequest}
import org.apache.pekko.util.ByteString
import org.jetbrains.annotations.TestOnly
import scala.jdk.CollectionConverters.*

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

  def metricsStream(
    addAttribute: String,
    scrapeRequest: Option[PrometheusScrapeRequest] = None)
  : fs2.Stream[fs2.Pure, fs2.Chunk[Byte]] =
    meter:
      val attributeInBraces = fs2.Chunk.array(s"{$addAttribute}".getBytes(UTF_8))
      val attributeWithComma = fs2.Chunk.array(s"$addAttribute,".getBytes(UTF_8))
      fs2.Stream.emit:
        metrics_[fs2.Chunk[Byte]](scrapeRequest)
      .through:
        byteChunksToLines[fs2.Pure, fs2.Chunk[Byte]](breakLinesLongerThan = Some(1024))
      .map: line =>
        if line.isEmpty || !Character.isLetter(line(0).toChar) then
          line
        else
          line.vectorIndexOf('{') match
            case -1 =>
              line.vectorIndexOf(' ') match
                case -1 => line
                case i =>
                  val (a, b) = line.splitAt(i)
                  a ++ attributeInBraces ++ b
            case i =>
              val (a, b) = line.splitAt(i + 1)
              a ++ attributeWithComma ++ b

  def metrics[ByteSeq: ByteSequence](scrapeRequest: Option[PrometheusScrapeRequest] = None)
  : ByteSeq =
    meter:
      metrics_[ByteSeq](scrapeRequest)

  private def metrics_[ByteSeq: ByteSequence](scrapeRequest: Option[PrometheusScrapeRequest] = None)
  : ByteSeq =
    val outputStream = new ByteSeqOutputStream(lastSize + lastSize / 5)
    outputStream.writeBytes(HeadlineBytes)
    textWriter.write(outputStream, registry.scrape(scrapeRequest.orNull))
    lastSize = outputStream.size()
    outputStream.byteSeq[ByteSeq]

  // Not used !!!
  private def metricsByteString(request: HttpRequest = HttpRequest(HttpMethods.GET, "/metrics"))
  : ByteString =
    val exchange = new PrometheusJmxAdapterForHttp.PekkoPromtheusExchange(request)
    scrapeHandler.handleRequest(exchange)
    exchange.metricsByteString


private[prometheus] object PrometheusJmxAdapter:
  private val logger = Logger[this.type]
  private val meter = CallMeter("PrometheusJmxAdapter")
  private val DefaultYamlResource = JavaResource("js7/service/prometheus/prometheus.yaml")
  private val DefaultYamlConfig = DefaultYamlResource.asUTF8String

  private val HeadlineBytes =
    s"""# Experimental metrics for Prometheus
       |
       |""".stripMargin.getBytes(UTF_8)

  @TestOnly
  val Headline = new String(HeadlineBytes, UTF_8)

  logger.trace("Available JMX MBeans:")
  ManagementFactory.getPlatformMBeanServer.queryNames(null, null).asScala.toArray
    .map(_.getCanonicalName).sorted.foreachWithBracket(): (o, br) =>
      logger.trace(s"$br$o")
